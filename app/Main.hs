{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Torrent
import TorrentFileSystem
import SyncTypes
import qualified Sync

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad (void, when, forM, join)
import Data.Bits (toIntegralSized)
import Data.List (stripPrefix)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
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

import Debug.Trace
import System.Posix (changeWorkingDirectoryFd, dupTo, closeFd)
import System.Posix.IO (defaultFileFlags)
import System.Posix (openFd)
import System.Directory (createDirectoryIfMissing)
import Args
import System.IO.Error (catchIOError)
import System.IO (hPutStr)
import System.IO (hGetContents')
import GHC.Conc (newTVarIO)
import Control.Concurrent.STM (mkWeakTVar, readTVarIO, atomically, readTVar, newTVar, writeTVar, modifyTVar)

type FuseFDType = TFSHandle

quotCeil x y = quot x y + if rem x y /= 0 then 1 else 0

staticStateDir = Fd 512

torrentStateName = ".torrent_state"
fuseStateName = ".fuse_state"

defaultStates :: Chan SyncEvent -> FuseTorrentArgs -> IO (FuseState, TorrentState)
defaultStates chan args = do
  newFiles <- newTVarIO Set.empty
  traceM "Opening root"
  rootDir <- openFd (fromMaybe (mountpoint args) $ stateDir args) ReadOnly defaultFileFlags { directory = True }
  _ <- dupTo rootDir staticStateDir
  closeFd rootDir
  let rootDir@(Fd rootDirFd') = staticStateDir
  traceM "Opening state dir"
  changeWorkingDirectoryFd rootDir
  createDirectoryIfMissing True $ mountpoint args
  let torrentStateDir' = "/proc/self/fd" </> show rootDirFd' </> torrentStateName
  traceM "State created"
  fs <- flip catchIOError (const $ return Nothing) $ withFileAt rootDir fuseStateName ReadOnly defaultFileFlags $ \f -> do
    c <- hGetContents' f
    return $ Just $ fmap uninitialized $ read $ seq c c
  traceShowM fs
  ref <- newTVarIO $ fromMaybe emptyFileSystem fs
  weak <- mkWeakTVar ref $ return ()
  lostFound <- if lostAndFound args then Just <$> newTVarIO emptyFileSystem else return Nothing
  wLostFound <- forM lostFound (`mkWeakTVar` return ())
  return (FuseState { _files = ref, _syncChannel = chan, _hiddenDirs = [], _newFiles = newFiles, _realStatePath = (rootDir, fuseStateName), _lostFound = lostFound }, TorrentState { _fuseFiles = weak, _statePath = torrentStateDir', _fuseLostFound = wLostFound })

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  null' <- openFd "/dev/null" ReadOnly defaultFileFlags
  _ <- dupTo null' staticStateDir
  closeFd null'
  options <- execParser programInfo
  traceShowM options
  traceShowM $ toFuseArgs options
  comChannel <- newChan
  (fuseState, torrState) <- defaultStates comChannel options
  let torrentMain = Sync.mainLoop comChannel torrState
  fuseRun "TorrentFuse" (toFuseArgs options) (myFuseFSOps fuseState torrentMain) defaultExceptionHandler

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
                                           , statBlocks = quotCeil (toInteger filesize) (maybe 1 toInteger blocksize)
                                           , statAccessTime = 0
                                           , statModificationTime = 0
                                           , statStatusChangeTime = 0
                                           }

applyMode :: FileMode -> FileStat -> FileStat
applyMode mode stat = stat { statFileMode = unionFileModes mode $ statFileMode stat }
applyOwnerWritable :: FileStat -> FileStat
applyOwnerWritable = applyMode ownerWriteMode

maybeLost :: FuseState -> FilePath -> IO (FilePath, TorrentFileSystemEntryList)
maybeLost state path =
  let maybeLost
        | Just rest <- stripPrefix "lost+found/" path,
          Just lostFoundActive <- state^.lostFound = Just (rest, readTVarIO lostFoundActive)
        | otherwise = Nothing
   in case maybeLost of
     Just (rest, readLostFound) -> (rest,) <$> readLostFound
     Nothing -> (path,) <$> (readTVarIO $ state^.files)

myFuseGetFileStat :: FuseState -> FilePath -> IO (Either Errno FileStat)
myFuseGetFileStat _ "/" = Right . dirStat <$> getFuseContext
myFuseGetFileStat _ "/lost+found" = Right . dirStat <$> getFuseContext
myFuseGetFileStat state ('/':path) = do
  ctx <- getFuseContext
  (path', files) <- maybeLost state path
  newFiles' <- readTVarIO $ state^.newFiles
  let matching = getTFS' files path'
  return $ case matching of
             Just (f, _) -> if isJust $ f^?contents
                                then Right $ dirStat ctx
                                else Right $ fileStat (f^?!filesize) (Just $ f^?!pieceSize) ctx
             _ -> if Set.member path newFiles'
                     then Right $ fileStat 0 Nothing ctx
                     else Left eNOENT

myFuseGetFileStat _ _ =
    return $ Left eNOENT

myFuseOpenDirectory :: FuseState -> FilePath -> IO Errno
myFuseOpenDirectory _ "/" = return eOK
myFuseOpenDirectory _ "/lost+found" = return eOK
myFuseOpenDirectory state ('/':path) = do
  (path', files) <- maybeLost state path
  let matching = getTFS' files path'
  return $ case matching of
             Just (f, _) -> if isJust $ f^?contents
                               then eOK
                               else eNOENT
             Nothing -> eNOENT

myFuseReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
myFuseReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readTVarIO $ state^.files
    traceShowM files
    newFiles <- readTVarIO $ state^.newFiles
    lostFound <- mapM readTVarIO $ state^.lostFound
    let lostFound' = if isJust lostFound then [("lost+found", dirStat ctx)] else []
        builtin = [(".", applyOwnerWritable $ dirStat ctx)
                  ,("..", dirStat ctx)] ++ lostFound'
        directories = flip map (Map.toList files) $ \(name, file) ->
          (name, case file^?filesize of
                   Just size -> fileStat size (file^?pieceSize) ctx
                   Nothing -> dirStat ctx)
        newFiles' = map (, applyOwnerWritable $ fileStat 0 Nothing ctx) $ Set.toList newFiles
    return $ Right $ traceShowId $ builtin ++ directories --  ++ newFiles'

myFuseReadDirectory state@FuseState { _lostFound = Just lf } "/lost+found" = do
    ctx <- getFuseContext
    files <- readTVarIO lf
    traceShowM files
    let builtin = [(".", applyOwnerWritable $ dirStat ctx)
                  ,("..", dirStat ctx)]
        directories = flip map (Map.toList files) $ \(name, file) ->
          (name, case file^?filesize of
                   Just size -> fileStat size (file^?pieceSize) ctx
                   Nothing -> dirStat ctx)
    return $ Right $ traceShowId $ builtin ++ directories

myFuseReadDirectory state ('/':path) = do
  ctx <- getFuseContext
  (path', files') <- maybeLost state path
  traceShowM files'

  let matching = getTFS files' path'
      builtin = [(".",          dirStat  ctx)
                ,("..",         dirStat  ctx)]
  return $ Right $ traceShowId $ builtin ++ map (\(name, t) -> (name, (if isJust (t^?contents) then dirStat else fileStat (t^?!filesize) (t^?pieceSize)) ctx)) matching

myFuseOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FuseFDType)
myFuseOpen fuseState ('/':path) ReadWrite _ = do
  (path', files) <- maybeLost fuseState path
  case getTFS files path' of
    [] -> atomically $ do
      files <- readTVar $ fuseState^.newFiles
      if Set.member path files
        then do
          writeTVar (fuseState^.newFiles) $ Set.delete path files
          return . NewTorrentFileHandle path <$> newTVar B.empty
        else return $ Left eACCES
    _ -> return $ Left eACCES

myFuseOpen fuseState path WriteOnly flags = myFuseOpen fuseState path ReadWrite flags

myFuseOpen _ _ ReadWrite _ = return $ Left eACCES
myFuseOpen fuseState ('/':path) ReadOnly flags = do
    (path', files) <- maybeLost fuseState path
    let matching = getTFS' files path'
    case matching of
      Just (fsEntry@TFSTorrentFile{TorrentFileSystem._torrent = torrent }, _) -> do
        uid <- newEmptyMVar
        writeChan (fuseState^.syncChannel) $ OpenTorrent { SyncTypes._torrent = torrent, _fdCallback = uid, _piece = fsEntry^?!pieceStart }
        uid' <- takeMVar uid
        buffer <- newTVarIO Nothing
        return $ Right $ TorrentFileHandle { _fileNoBlock = nonBlock flags
                                           , _tfsEntry = traceShowId fsEntry
                                           , _blockCache = buffer
                                           , _uid = uid' }
      _ -> return $ Left eNOENT

myFuseRename :: FuseState -> FilePath -> FilePath -> IO Errno
myFuseRename fuseState ('/':from) ('/':to)
   | Just _ <- fuseState^.lostFound,
     Just _ <- inLostFound to = return eNOTSUP
   | Just lost <- fuseState^.lostFound,
     Just fromLost <- inLostFound from = do
       atomically $ do
         lostFiles <- readTVar lost
         found <- case takeTFS lostFiles fromLost of
           Just (found, new) -> do
             writeTVar lost new
             return $ Just found
           Nothing -> return Nothing
         tFiles <- readTVar $ fuseState^.files
         case myFuseRename'' (fmap (,tFiles) found) of
                                Left err -> return err
                                Right files' -> do
                                  writeTVar (fuseState^.files) $ files'
                                  return eOK
   | otherwise = atomically myFuseRename'
  where myFuseRename' = do
          allFiles <- readTVar (fuseState^.files)
          case myFuseRename'' $ takeTFS allFiles from of
                                Left err -> return err
                                Right files' -> do
                                  writeTVar (fuseState^.files) files'
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

myFuseCreateDevice :: FuseState -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
myFuseCreateDevice state ('/':path) RegularFile _ _ =
  if takeExtension path == ".torrent"
     then (atomically $ modifyTVar (state^.newFiles) (Set.insert path)) >> return eOK
     else return eACCES
myFuseCreateDevice _ _ _ _ _ = return eACCES

myFuseCreateDirectory :: FuseState -> FilePath -> FileMode -> IO Errno
myFuseCreateDirectory _ "/lost+found" _ = return eACCES
myFuseCreateDirectory fuseState ('/':path) _ = do
  atomically $ modifyTVar (fuseState^.files) $ \files -> let newDir = pathToTFSDir' path
                                                 in (mergeDirectories2 files $ New newDir)
  return eOK
myFuseCreateDirectory _ _ _ = return eACCES

-- myFuseReadSymbolicLink :: FuseState -> FilePath -> IO (Either Errno FilePath)
-- myFuseReadSymbolicLink s p
--   | p ==

myFuseRead :: FuseState -> FilePath -> FuseFDType -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
myFuseRead fuseState _ handle@SimpleFileHandle{} count offset = do
  pos <- hTell $ handle^?!fileHandle
  when (pos /= toInteger offset) $ hSeek (handle^?!fileHandle) AbsoluteSeek $ toInteger offset
  Right <$> B.hGet (handle^?!fileHandle) (fromJust $ toIntegralSized count)

myFuseRead _ _ (NewTorrentFileHandle _ buffer) count offset =  Right . B.take (fromJust $ toIntegralSized count) . B.drop (fromJust $ toIntegralSized offset) <$> readTVarIO buffer

myFuseRead fuseState _ TorrentFileHandle{ _tfsEntry = tfs, _blockCache = blockCache, _uid = fd, _fileNoBlock = fileNoBlock } count offset = do
  let count' = toInteger count
      pieceStart' = toInteger $ tfs^?!pieceStart
      pieceStartOffset' = toInteger $ tfs^?!pieceStartOffset
      offset' = toInteger offset
      pieceSize' = toInteger $ tfs^?!pieceSize
      totalFirstPieceOffset = pieceStartOffset' + offset'
      firstPiece' = pieceStart' + quot totalFirstPieceOffset pieceSize'
      actualFirstPieceOffset = mod totalFirstPieceOffset pieceSize'
      spaceInFirstPiece = pieceSize' - actualFirstPieceOffset
      afterFirstPiece = max 0 $ count' - spaceInFirstPiece
      additionalPieces = quotCeil afterFirstPiece pieceSize'
      pieces = 1 + additionalPieces
      fittingCount = toInteger (tfs^?!filesize) - offset'
  maybeFirstBlock <- readTVarIO blockCache
  let maybeFirstBlock' = case maybeFirstBlock of
                           Just (cachePiece, cacheBuf) ->
                             if toInteger cachePiece == firstPiece'
                               then Just cacheBuf
                               else Nothing
                           _ -> Nothing
      firstBlockModifier = case maybeFirstBlock' of
                             Just _ -> 1
                             Nothing -> 0
      numPieces = pieces - firstBlockModifier
      firstFetchedPiece = firstPiece' + firstBlockModifier
  retChans <- mapM (\piece -> newEmptyMVar >>= \chan -> return (chan, piece)) $ take (fromJust $ toIntegralSized numPieces) [firstFetchedPiece..]
  case tfs^?TorrentFileSystem.torrent of
    Nothing -> return $ Left eAGAIN
    Just torrent -> do
      let req = ReadTorrent { SyncTypes._torrent = torrent
                            , _fd = fd
                            , _piece = fromJust $ toIntegralSized firstFetchedPiece
                            , _fileData = map (^._1) retChans }
      writeChan (fuseState^.syncChannel) req
      let handleResponse = do
            returnedData <- forM retChans $ \(chan, piece) -> (fromJust $ toIntegralSized piece,) <$> takeMVar chan
            when (numPieces > 0) $ atomically $ writeTVar blockCache $ Just $ last returnedData
            let unnumberedData = map (^._2) returnedData
            let wantedData = B.take (fromJust $ toIntegralSized fittingCount) $ B.drop (fromJust $ toIntegralSized actualFirstPieceOffset) $ B.concat $ maybe unnumberedData (:unnumberedData) maybeFirstBlock'
            return $ Right wantedData
      if fileNoBlock
         then timeout 100000 handleResponse >>= \case
                Just d -> return d
                Nothing -> return $ Left eWOULDBLOCK
         else handleResponse

myFuseWrite :: FuseState -> FilePath -> FuseFDType -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
myFuseWrite state _ fh@(NewTorrentFileHandle _ buffer) input offset = atomically $ do
  buffer' <- readTVar buffer
  if Just offset == toIntegralSized (B.length buffer')
     then do
       writeTVar buffer $ B.append buffer' input
       return $ Right $ fromJust $ toIntegralSized $ B.length input
     else return $ Left eWOULDBLOCK

myFuseRemoveDirectory :: FuseState -> FilePath -> IO Errno
myFuseRemoveDirectory fuseState ('/':path) = atomically $ do
  files' <- readTVar (fuseState^.files)
  case takeTFS files' path of
    (Just (TFSDir { _contents = c}, n)) ->
      if null c then do
          writeTVar (fuseState^.files) n
          return eOK
        else return eNOTEMPTY
    _ -> return eNOTSUP

myFuseRemoveLink :: FuseState -> FilePath -> IO Errno
myFuseRemoveLink fuseState ('/':path) = do
  file <- atomically $ do
    files' <- readTVar (fuseState^.files)
    case takeTFS files' path of
      f@(Just (TFSTorrentFile {}, n)) -> do
        writeTVar (fuseState^.files) n
        return f
      _ -> return Nothing
  let sendRemove torrent = writeChan (fuseState^.syncChannel) (RemoveTorrent torrent) >> return eOK
      file'
        | Just (f, n) <- file,
          Just hot <- assertHotBackend f = Just (hot, n)
        | otherwise = file
  case traceShowId $ file' of
    Just (TFSTorrentFile { TorrentFileSystem._torrent = torrent, _singleFileTorrent = True }, _) -> sendRemove torrent

    Just (TFSTorrentFile { TorrentFileSystem._torrent = t, TorrentFileSystem._hash = h }, fs) -> let lastFile = isNothing $ filterFS (\t -> (Just h) == (t^? hash)) $ Map.toList fs
     in if lastFile then sendRemove t
                    else return eOK
    _ -> return eNOENT

myFuseRelease :: FuseState -> FilePath -> FuseFDType -> IO ()
myFuseRelease _ _ fh@SimpleFileHandle{} = hClose $ fh^?!fileHandle

myFuseRelease fuseState _ fh@TorrentFileHandle{} = case fh^?!tfsEntry^?TorrentFileSystem.torrent of
  Just torrent -> writeChan (fuseState^.syncChannel) $ CloseTorrent { SyncTypes._torrent = torrent, _fd = fh^?!uid }
  Nothing -> return ()

myFuseRelease fuseState _ fh@(NewTorrentFileHandle path content) =
  readTVarIO content >>= writeChan (fuseState^.syncChannel) . AddTorrent (Just $ takeDirectory path) . NewTorrentFile

myFuseDestroy :: FuseState -> IO ()
myFuseDestroy fuseState = do
  torrentDead <- newEmptyMVar
  writeChan (fuseState^.syncChannel) $ FuseDead torrentDead
  let (fd, state) = fuseState^.realStatePath
  files <- readTVarIO $ fuseState^.files
  flip catchIOError traceShowM $
    withFileAt fd state WriteOnly defaultFileFlags { creat = Just 0o700, trunc = True } $
      \f -> hPutStr f $ show $ intoStoredTorrent files
  void $ takeMVar torrentDead
  traceM "Torrent is dead, finalizing"
