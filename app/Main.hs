{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import Torrent
import TorrentFileSystem as TFS
import SyncTypes
import qualified Sync

import Control.Concurrent
import Control.Exception
import Control.Monad (void, when, forM)
import Data.Bits (toIntegralSized)
import Data.List (stripPrefix)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe, mapMaybe)
import Foreign.C.Error
import System.FilePath
import System.Fuse
import System.IO (hClose, hTell, hSeek, SeekMode(AbsoluteSeek))
import System.Posix.Files
import System.Posix.Types
import System.Timeout (timeout)
import Sync hiding (mainLoop)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.Posix (changeWorkingDirectoryFd, dupTo, closeFd)
import System.Posix.IO (defaultFileFlags)
import System.Posix (openFd)
import System.Directory (createDirectoryIfMissing)
import qualified Args
import System.IO.Error (catchIOError)
import System.IO (hPutStr)
import System.IO (hGetContents')
import Control.Concurrent.STM (mkWeakTVar, readTVarIO, atomically, readTVar, newTVar, writeTVar, modifyTVar, STM, TVar, newTVarIO, newTChan, TChan, writeTChan, dupTChan, swapTVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString as BS
import qualified Control.Monad.STM as STM
import Utils ((/^), (/.), (/%), OptionalTrace (..), OptionalDebug (..), withTrace)

type FuseFDType = TFSHandle

staticStateDir = Fd 512

torrentStateName = ".torrent_state"
fuseStateName = ".fuse_state"

defaultStates :: TChan SyncEvent -> Args.FuseTorrentArgs -> IO (FuseState, TorrentState)
defaultStates chan args = do
  let debug = if Args.debug args
                then Trace
                else NoTrace
  newFiles <- newTVarIO Set.empty
  traceM debug "Opening root"
  rootDir <- openFd (fromMaybe (Args.mountpoint args) $ Args.stateDir args) ReadOnly defaultFileFlags { directory = True }
  _ <- dupTo rootDir staticStateDir
  closeFd rootDir
  let rootDir@(Fd rootDirFd') = staticStateDir
  traceM debug "Opening state dir"
  changeWorkingDirectoryFd rootDir
  createDirectoryIfMissing True $ Args.mountpoint args
  let torrentStateDir' = "/proc/self/fd" </> show rootDirFd' </> torrentStateName
  traceM debug "State created"
  fs <- flip catchIOError (const $ return Nothing) $ withFileAt rootDir fuseStateName ReadOnly defaultFileFlags $ \f -> do
    c <- hGetContents' f
    return $ Just $ fmap uninitialized $ read $ seq c c
  traceShowM debug fs
  ref <- newTVarIO $ fromMaybe emptyFileSystem fs
  weak <- mkWeakTVar ref $ return ()
  lostFound <- if Args.lostAndFound args then Just <$> newTVarIO emptyFileSystem else return Nothing
  wLostFound <- forM lostFound (`mkWeakTVar` return ())
  return (FuseState { _files = ref, _syncChannel = chan, _hiddenDirs = [], _newFiles = newFiles, _realStatePath = (rootDir, fuseStateName), _lostFound = lostFound, _nonBlockTimeout = Args.nonBlockTimeout args, _fuseTrace = debug }, TorrentState { _fuseFiles = weak, _statePath = torrentStateDir', _fuseLostFound = wLostFound, _torrentTrace = debug })

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  null' <- openFd "/dev/null" ReadOnly defaultFileFlags
  _ <- dupTo null' staticStateDir
  closeFd null'
  options <- Args.execParser Args.programInfo
  let traceTo = withTrace $ Args.debug options
  traceShowM traceTo options
  traceShowM traceTo $ Args.toFuseArgs options
  (comChannel, comChannel') <- atomically $ do
    new <- newTChan
    second <- dupTChan new
    return (new, second)
  (fuseState, torrState) <- defaultStates comChannel options
  let torrentMain = Sync.mainLoop comChannel' torrState
  fuseRun "TorrentFuse" (Args.toFuseArgs options) (myFuseFSOps fuseState torrentMain) defaultExceptionHandler

myFuseFSOps :: FuseState -> IO ThreadId -> FuseOperations FuseFDType
myFuseFSOps state main' = defaultFuseOps { fuseGetFileStat     = myFuseGetFileStat state
                                        , fuseOpen            = myFuseOpen state
                                        , fuseRename          = myFuseRename state
                                        , fuseCreateDevice    = myFuseCreateDevice state
                                        , fuseCreateDirectory = myFuseCreateDirectory state
                                        , fuseRead            = myFuseRead state
                                        , fuseWrite           = myFuseWrite state
                                        , fuseRemoveDirectory = myFuseRemoveDirectory state
                                        , fuseRemoveLink      = myFuseRemoveLink state
                                        , fuseRelease         = myFuseRelease state
                                        , fuseOpenDirectory   = myFuseOpenDirectory state
                                        , fuseReadDirectory   = myFuseReadDirectory state
                                        , fuseInit            = void main'
                                        , fuseDestroy         = myFuseDestroy state
                                        }
dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerWriteMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

unknownFileStat :: FuseContext -> FileStat
unknownFileStat ctx = FileStat { statEntryType = Unknown
                                           , statFileMode = foldr1 unionFileModes
                                                              [ ownerReadMode
                                                              , groupReadMode
                                                              , otherReadMode
                                                              ]
                                           , statLinkCount = 1
                                           , statFileOwner = fuseCtxUserID ctx
                                           , statFileGroup = fuseCtxGroupID ctx
                                           , statSpecialDeviceID = 0
                                           , statFileSize = 0
                                           , statBlocks = 0
                                           , statAccessTime = 0
                                           , statModificationTime = 0
                                           , statStatusChangeTime = 0
                                           }

fileStat :: Integral a => FileOffset -> Maybe a -> FuseContext -> FileStat
fileStat filesize blocksize ctx = FileStat { statEntryType = RegularFile
                                           , statFileMode = foldr1 unionFileModes
                                                              [ ownerReadMode
                                                              , groupReadMode
                                                              , otherReadMode
                                                              ]
                                           , statLinkCount = 1
                                           , statFileOwner = fuseCtxUserID ctx
                                           , statFileGroup = fuseCtxGroupID ctx
                                           , statSpecialDeviceID = 0
                                           , statFileSize = filesize
                                           , statBlocks = (toInteger filesize) /^ (maybe 1 toInteger blocksize)
                                           , statAccessTime = 0
                                           , statModificationTime = 0
                                           , statStatusChangeTime = 0
                                           }

applyMode :: FileMode -> FileStat -> FileStat
applyMode mode stat = stat { statFileMode = unionFileModes mode $ statFileMode stat }
applyOwnerWritable :: FileStat -> FileStat
applyOwnerWritable = applyMode ownerWriteMode

maybeLostWith :: Monad m => (TVar TorrentFileSystemEntryList -> m TorrentFileSystemEntryList) -> FuseState -> FilePath -> m (FilePath, TorrentFileSystemEntryList)
maybeLostWith readVar state path =
  let maybeLost
        | Just rest <- stripPrefix "lost+found/" path,
          Just lostFoundActive <- _lostFound state = Just (rest, readVar lostFoundActive)
        | otherwise = Nothing
   in case maybeLost of
     Just (rest, readLostFound) -> (rest,) <$> readLostFound
     Nothing -> (path,) <$> (readVar $ _files state)

maybeLost :: FuseState -> FilePath -> STM (FilePath, TorrentFileSystemEntryList)
maybeLost = maybeLostWith readTVar

maybeLostIO :: FuseState -> FilePath -> IO (FilePath, TorrentFileSystemEntryList)
maybeLostIO = maybeLostWith readTVarIO

myFuseGetFileStat :: FuseState -> FilePath -> IO (Either Errno FileStat)
myFuseGetFileStat _ "/" = Right . dirStat <$> getFuseContext
myFuseGetFileStat _ "/lost+found" = Right . dirStat <$> getFuseContext
myFuseGetFileStat state ('/':path) = do
  ctx <- getFuseContext
  (path', files) <- maybeLostIO state path
  newFiles' <- readTVarIO $ _newFiles state
  let matching = getTFS' files path'
  return $ case matching of
             Just (TFSUninitialized _, _) -> Right $ fileStat 0 Nothing ctx
             Just (TFSDir {}, _) -> Right $ dirStat ctx
             Just (TFSTorrentFile {TFS._filesize = fs, TFS._pieceSize = bs}, _) -> Right $ fileStat fs (Just bs) ctx
             Just (TFSFile {}, _) -> undefined
             _ -> if Set.member path newFiles'
                     then Right $ fileStat 0 Nothing ctx
                     else Left eNOENT

myFuseGetFileStat _ _ =
    return $ Left eNOENT

myFuseOpenDirectory :: FuseState -> FilePath -> IO Errno
myFuseOpenDirectory _ "/" = return eOK
myFuseOpenDirectory _ "/lost+found" = return eOK
myFuseOpenDirectory state ('/':path) = do
  (path', files) <- maybeLostIO state path
  let matching = getTFS' files path'
  return $ case matching of
             Just (TFSDir {}, _) -> eOK
             Just (TFSUninitialized (TFSDir {}), _) -> eOK
             _ -> eNOENT
myFuseOpenDirectory _ _ = return eNOENT

myFuseReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
myFuseReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readTVarIO $ _files state
    traceShowM state files
    -- newFiles <- readTVarIO $ _newFiles state
    lostFound <- mapM readTVarIO $ _lostFound state
    let lostFound' = if isJust lostFound then [("lost+found", dirStat ctx)] else []
        builtin = [(".", applyOwnerWritable $ dirStat ctx)
                  ,("..", dirStat ctx)] ++ lostFound'
        directories = flip map (Map.toList files) $ \(name, file) ->
          (name, case file of
                   TFSTorrentFile { TFS._filesize = s, TFS._pieceSize = ps } -> fileStat s (Just ps) ctx
                   TFSUninitialized (TFSTorrentFile { TFS._filesize = s, TFS._pieceSize = ps }) -> fileStat s (Just ps) ctx
                   TFSDir {} -> dirStat ctx
                   _ -> unknownFileStat ctx)
        -- newFiles' = map (, applyOwnerWritable $ fileStat 0 Nothing ctx) $ Set.toList newFiles
    return $ Right $ traceShowId state $ builtin ++ directories --  ++ newFiles'

myFuseReadDirectory state@FuseState { _lostFound = Just lf } "/lost+found" = do
    ctx <- getFuseContext
    files <- readTVarIO lf
    traceShowM state files
    let builtin = [(".", applyOwnerWritable $ dirStat ctx)
                  ,("..", dirStat ctx)]
                  -- TODO Handle files that has not been matched with a torrent
        directories = flip map (Map.toList files) $ \(name, file) ->
          (name, case file of
                   TFSUninitialized _ -> unknownFileStat ctx
                   TFSTorrentFile {TFS._pieceSize = bs, TFS._filesize = fs} -> fileStat fs (Just bs) ctx
                   TFSFile { TFS._filesize = fs } -> fileStat fs Nothing ctx
                   TFSDir {} -> dirStat ctx)
    return $ Right $ traceShowId state $ builtin ++ directories

myFuseReadDirectory state ('/':path) = do
  ctx <- getFuseContext
  (path', files') <- maybeLostIO state path
  traceShowM state files'

  let matching = getTFS files' path'
      builtin = [(".",          dirStat  ctx)
                ,("..",         dirStat  ctx)]
      toEntry (TFSUninitialized _) = unknownFileStat ctx
      toEntry TFSDir {} = dirStat ctx
      toEntry TFSFile {} = undefined
      toEntry TFSTorrentFile {TFS._filesize = fs, TFS._pieceSize = bs} = fileStat fs (Just bs) ctx

  return $ Right $ traceShowId state $ builtin ++ map (\(name, e) -> (name, toEntry e)) matching
myFuseReadDirectory _ _ = return $ Left eNOENT

myFuseOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FuseFDType)
myFuseOpen fuseState ('/':path) ReadWrite _ = atomically $ do
  (path', files) <- maybeLost fuseState path
  case getTFS files path' of
    [] -> do
      files <- readTVar $ _newFiles fuseState
      if Set.member path files
        then do
          writeTVar (_newFiles fuseState) $ Set.delete path files
          return . NewTorrentFileHandle path <$> newTVar B.empty
        else return $ Left eACCES
    _ -> return $ Left eACCES

myFuseOpen fuseState path WriteOnly flags = myFuseOpen fuseState path ReadWrite flags

myFuseOpen _ _ ReadWrite _ = return $ Left eACCES
myFuseOpen fuseState ('/':path) ReadOnly flags = do
    (path', files) <- maybeLostIO fuseState path
    let matching = getTFS' files path'
    case matching of
      Just (fsEntry@TFSTorrentFile{TFS._torrent = torrent, TFS._pieceStart = pieceStart }, _) -> do
        uid <- liftIO $ newEmptyMVar
        atomically $ writeTChan (_syncChannel fuseState) $ OpenTorrent { SyncTypes._torrent = torrent, _fdCallback = uid, _piece = pieceStart }
        uid' <- takeMVar uid
        buffer <- newTVarIO []
        lastReq <- newTVarIO Nothing
        return $ Right $ TorrentFileHandle { _fileNoBlock = nonBlock flags
                                           , _tfsEntry = traceShowId fuseState $ fsEntry
                                           , _blockCache = buffer
                                           , _uid = uid'
                                           , _lastRequest = lastReq}
      _ -> return $ Left eNOENT
myFuseOpen _ _ _ _ = return $ Left eNOENT

myFuseRename :: FuseState -> FilePath -> FilePath -> IO Errno
myFuseRename fuseState ('/':from) ('/':to)
   | Just _ <- _lostFound fuseState,
     Just _ <- inLostFound to = return eNOTSUP
   | Just lost <- _lostFound fuseState,
     Just fromLost <- inLostFound from = do
       atomically $ do
         lostFiles <- readTVar lost
         found <- case takeTFS lostFiles fromLost of
           Just (found, new) -> do
             writeTVar lost new
             return $ Just found
           Nothing -> return Nothing
         tFiles <- readTVar $ _files fuseState
         case myFuseRename'' (fmap (,tFiles) found) of
                                Left err -> return err
                                Right files' -> do
                                  writeTVar (_files fuseState) $ files'
                                  return eOK
   | otherwise = atomically myFuseRename'
  where myFuseRename' = do
          allFiles <- readTVar (_files fuseState)
          case myFuseRename'' $ takeTFS allFiles from of
                                Left err -> return err
                                Right files' -> do
                                  writeTVar (_files fuseState) files'
                                  return eOK
        inLostFound = stripPrefix "lost+found/"
        last [] = Nothing
        last [x] = Just x
        last (_:x) = last x
        filename = last . splitDirectories
        myFuseRename'' :: Maybe (TorrentFileSystemEntry HotTorrent, TorrentFileSystemEntryList) -> Either Errno TorrentFileSystemEntryList
        myFuseRename'' files
          | Just (f, files') <- files,
            Just newFile <- toTFSDir to f = Right $ mergeDirectories $ Map.toList files' ++ Map.toList newFile
          | Just (f, files') <- files,
            Just filename' <- filename from = Right $ mergeDirectories $ Map.toList files' ++ [(filename', f)]
          | Nothing <- files = Left eNOENT
          | otherwise = Left eINVAL
myFuseRename _ _ _ = return eNOENT

myFuseCreateDevice :: FuseState -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
myFuseCreateDevice state ('/':path) RegularFile _ _ =
  if takeExtension path == ".torrent"
     then (atomically $ modifyTVar (_newFiles state) (Set.insert path)) >> return eOK
     else return eACCES
myFuseCreateDevice _ _ _ _ _ = return eACCES

myFuseCreateDirectory :: FuseState -> FilePath -> FileMode -> IO Errno
myFuseCreateDirectory _ "/lost+found" _ = return eACCES
myFuseCreateDirectory fuseState ('/':path) _ = do
  atomically $ modifyTVar (_files fuseState) $ \files -> let newDir = pathToTFSDir' path
                                                 in (mergeDirectories2 files $ New newDir)
  return eOK
myFuseCreateDirectory _ _ _ = return eACCES

-- myFuseReadSymbolicLink :: FuseState -> FilePath -> IO (Either Errno FilePath)
-- myFuseReadSymbolicLink s p
--   | p ==

myFuseRead :: FuseState -> FilePath -> FuseFDType -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
myFuseRead _ _ SimpleFileHandle{_fileHandle = fHandle} count offset = do
  pos <- hTell fHandle
  when (pos /= toInteger offset) $ hSeek fHandle AbsoluteSeek $ toInteger offset
  Right <$> B.hGet fHandle (fromJust $ toIntegralSized count)

myFuseRead _ _ (NewTorrentFileHandle _ buffer) count offset =  Right . B.take (fromJust $ toIntegralSized count) . B.drop (fromJust $ toIntegralSized offset) <$> readTVarIO buffer

myFuseRead fuseState _ TorrentFileHandle{ _tfsEntry = TFSTorrentFile {TFS._torrent = torrHandle, TFS._pieceStart = pieceStart, TFS._pieceSize = pieceSize, TFS._pieceStartOffset = pieceStartOffset, TFS._filesize = filesize}, _blockCache = blockCache, _uid = fd, _fileNoBlock = fileNoBlock, _lastRequest = prev } count offset = do
  lastRequest' <- atomically $ swapTVar prev $ Just offset
  -- let count' = toInteger count
  --     offset' = toInteger offset
  --     pieceSize' = fromIntegral pieceSize
  --     totalFirstPieceOffset = fromIntegral pieceStartOffset + offset'
  --     firstPiece' = fromIntegral pieceStart + quot totalFirstPieceOffset pieceSize'
  --     actualFirstPieceOffset = mod totalFirstPieceOffset pieceSize'
  --     spaceInFirstPiece = pieceSize' - actualFirstPieceOffset
  --     afterFirstPiece = max 0 $ count' - spaceInFirstPiece
  --     additionalPieces = quotCeil afterFirstPiece pieceSize'
  --     pieces = 1 + additionalPieces
  --     fittingCount = toInteger (tfs^?!filesize) - offset'

  let offset' = toInteger offset
      count' = max 0 (min (fromIntegral filesize - offset') $ toInteger count)
      pieceSize' = fromIntegral pieceSize
      pieceStartOffset' = fromIntegral pieceStartOffset
      (firstPiece, pieceOffset') = (\(q, m) -> (q + fromIntegral pieceStart, m)) $ (offset' + pieceStartOffset') /% pieceSize'
      p@(piecesCount, _) = (count' + pieceOffset') /% pieceSize'
      piecesCount'
        | (0, 0) <- p = 0
        | (q, 0) <- p = q - 1
        | otherwise = piecesCount
      currentReadLastPiece = firstPiece + piecesCount'
      _fileLastPiece = (fromIntegral filesize /. pieceSize') - (pieceStartOffset' /^ pieceSize')
      wantedPieces
        | count' == 0 = []
        | otherwise = [firstPiece..currentReadLastPiece]

  desiredBlocks <- atomically $ do
    cachedBlocks <- readTVar blockCache
    let takeMatching hasPiece wantsPiece
          | (p1, c):hx <- hasPiece,
            p2:wx <- wantsPiece,
            fromIntegral p1 == fromIntegral p2 = (Right (p1, c)):takeMatching hx wx
          | (p1, _):hx <- hasPiece,
            wa@(p2:_) <- wantsPiece,
            fromIntegral p1 == fromIntegral p2 = takeMatching hx wa
          | wanted:wx <- wantsPiece = Left wanted:takeMatching hasPiece wx
          | otherwise = []
    return $ takeMatching cachedBlocks wantedPieces
  let takeUncached (Left uncached) = Just uncached
      takeUncached _ = Nothing
      takeCached (Right cached) = Just cached
      takeCached _ = Nothing
      requestPieces = mapMaybe (fmap fromIntegral . takeUncached) desiredBlocks
      zippedCachedPieces = mapMaybe takeCached desiredBlocks
      cachedPieces = fmap snd zippedCachedPieces
      fetchTimeout = fromIntegral $ _nonBlockTimeout fuseState
      fetchAdditionalBackground = fetchAdditional (zippedCachedPieces ++) requestPieces
      fileNoBlock'
        | Just fo <- lastRequest',
          fo == offset = False
        | otherwise = fileNoBlock
      maybeFetch strictNoBlock
        | null requestPieces = pure $ Right $ B.concat cachedPieces
        | fileNoBlock',
          not strictNoBlock = do
            t <- newTVarIO Nothing
            _ <- liftIO $ forkIO $ do
              traceShowM fuseState $ "Forking for result"
              additional <- fetchAdditionalBackground
              atomically $ writeTVar t $ Just additional
              return ()
            result <- atomically $ readTVar t >>= \case
              Just newFetched -> return newFetched
              Nothing -> STM.retry
            return $ Right $ B.concat $ cachedPieces ++ result
        | fileNoBlock' = do
            _ <- liftIO $ forkIO $ void $ fetchAdditionalBackground
            return $ Left eWOULDBLOCK
        | otherwise = do
            let lastElem [] = []
                lastElem [a] = [a]
                lastElem (_:xs) = lastElem xs
            fetched <- fetchAdditional lastElem requestPieces
            return $ Right $ B.concat $ cachedPieces ++ fetched
      requestedData = maybeFetch (fetchTimeout == 0) >>= return . fmap (B.take (fromIntegral count') . B.drop (fromIntegral pieceOffset'))
      fetchAdditional saveCache requestPieces@(firstFetchedPiece:_) = do
        pieceChan <- mapM (const $ newTorrentReadCallback) requestPieces
        -- wPieceChan <- mapM (`mkWeakTVar` pure ()) pieceChan
        let req = ReadTorrent { SyncTypes._torrent = torrHandle
                              , _fd = fd
                              , _count = 1
                              , _piece = fromIntegral firstFetchedPiece
                              , _pieceData = fmap snd pieceChan }
        atomically $ writeTChan (_syncChannel fuseState) req
        traceShowM fuseState $ "Waiting for replies for " ++ show requestPieces
        replies <- atomically $ mapM (readTorrentReadCallback . fst) pieceChan
        atomically $ writeTVar blockCache $ saveCache $ zip requestPieces replies
        return replies
      fetchAdditional _ [] = undefined

        -- TODO This must return the exact size of bytes requested by the user 😫
        -- returnedData <- forM retChans $ \(chan, piece) -> (fromJust $ toIntegralSized piece,) <$> takeMVar chan
        -- traceShowM fuseState ("Received pieces", numPieces)
        -- when (numPieces > 0) $ atomically $ writeTVar blockCache $ Just $ last returnedData
        -- let unnumberedData = map (^._2) returnedData
        -- let wantedData = B.take (fromJust $ toIntegralSized fittingCount) $ B.drop (fromJust $ toIntegralSized actualFirstPieceOffset) $ B.concat $ maybe unnumberedData (:unnumberedData) maybeFirstBlock'
        -- return $ Right wantedData
  let requestedData' = do
        repl <- requestedData
        traceShowM fuseState (("count", count, "noBlock", fileNoBlock', lastRequest'), ("pieceSize", pieceSize, "offset", offset, "pieceOffset", pieceOffset'), ("firstPiece", firstPiece, "lastPiece", currentReadLastPiece, "request", requestPieces, "useCache", length cachedPieces, "length", case repl of Left _ -> "err"; Right l -> show $ B.length l))
        return $ repl

  if fileNoBlock' && fetchTimeout /= 0
     then timeout fetchTimeout requestedData' >>= \case
            Just d -> return d
            Nothing -> return $ Left eWOULDBLOCK
     else requestedData'
myFuseRead _ _ _ _ _ = return $ Left eBADF

myFuseWrite :: FuseState -> FilePath -> FuseFDType -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
myFuseWrite _ _ (NewTorrentFileHandle _ buffer) input offset = atomically $ do
  buffer' <- readTVar buffer
  if Just offset == toIntegralSized (B.length buffer')
     then do
       writeTVar buffer $ B.append buffer' input
       return $ Right $ fromJust $ toIntegralSized $ B.length input
     else return $ Left eWOULDBLOCK
myFuseWrite _ _ _ _ _ = return $ Left eBADF

myFuseRemoveDirectory :: FuseState -> FilePath -> IO Errno
myFuseRemoveDirectory fuseState ('/':path) = atomically $ do
  files' <- readTVar $ _files fuseState
  case takeTFS files' path of
    (Just (TFSDir { _contents = c}, n)) ->
      if null c then do
          writeTVar (_files fuseState) n
          return eOK
        else return eNOTEMPTY
    _ -> return eNOTSUP
myFuseRemoveDirectory _ _ = return eNOENT

myFuseRemoveLink :: FuseState -> FilePath -> IO Errno
myFuseRemoveLink fuseState ('/':path) = do
  action <- atomically $ do
    files' <- readTVar $ _files fuseState
    let sendRemove torrent = traceShowM fuseState ("Requesting removal of torrent", torrent) >> writeTChan (_syncChannel fuseState) (RemoveTorrent torrent) >> return eOK
        file' from
          | Just (f, n) <- from,
            Just hot <- assertHotBackend f = Just (hot, n)
          | otherwise = from
    case file' $ takeTFS files' path of
      Just (TFSTorrentFile { TFS._torrent = torrent, _singleFileTorrent = True }, _) -> return $ sendRemove torrent

      Just (TFSTorrentFile { TFS._torrent = t, TFS._hash = h }, fs) -> do
        let filter' = \case
              TFSTorrentFile { _hash = h' } -> h' == h
              TFSUninitialized (TFSTorrentFile { _hash = h' }) -> h' == h
              _ -> False
            lastFile = isNothing $ filterFS filter' $ Map.toList fs
        writeTVar (_files fuseState) fs
        if lastFile then return $ sendRemove t
                      else return $ return eOK
      Just (TFSUninitialized (TFSTorrentFile {}), fs) -> do
        writeTVar (_files fuseState) fs
        return $ return eOK
      _ -> return $ return eNOENT
  atomically action
myFuseRemoveLink _ _ = return eNOENT

myFuseRelease :: FuseState -> FilePath -> FuseFDType -> IO ()
myFuseRelease _ _ SimpleFileHandle{_fileHandle = fh} = hClose fh

myFuseRelease fuseState _ TorrentFileHandle{_uid = uid, _tfsEntry = TFSTorrentFile { TFS._torrent = torrent }} = atomically $ writeTChan (_syncChannel fuseState) $ CloseTorrent { SyncTypes._torrent = torrent, _fd = uid }
myFuseRelease _ _ TorrentFileHandle{_tfsEntry = _ } = return ()

myFuseRelease fuseState _ (NewTorrentFileHandle path content) = do
  content' <- readTVarIO content
  traceShowM fuseState ("Torrent closed with size", BS.length content') -- was 479376 for ubuntu
  atomically $ writeTChan (_syncChannel fuseState) $ AddTorrent (Just $ takeDirectory path) $ NewTorrentFile content'

myFuseDestroy :: FuseState -> IO ()
myFuseDestroy fuseState = do
  torrentDead <- newEmptyMVar
  atomically $ writeTChan (_syncChannel fuseState) $ FuseDead torrentDead
  let (fd, state) = _realStatePath fuseState
  files <- readTVarIO $ _files fuseState
  flip catchIOError (traceShowM fuseState) $
    withFileAt fd state WriteOnly defaultFileFlags { creat = Just 0o700, trunc = True } $
      \f -> hPutStr f $ show $ intoStoredTorrent files
  void $ takeMVar torrentDead
  traceM fuseState "Torrent is dead, finalizing"
