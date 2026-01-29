{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import qualified Args
import Control.Concurrent
import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, mkWeakTVar, modifyTVar, newTChan, newTVar, newTVarIO, readTMVar, readTVar, readTVarIO, swapTVar, tryReadTMVar, writeTChan, writeTVar)
import Control.Exception
import Control.Monad (forM, void, when, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM()
import Data.Bits (toIntegralSized)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import qualified Debug.Trace as DebugTrace
import Foreign.C.Error
import Foreign.C.Types (CInt)
import Sync hiding (mainLoop)
import qualified Sync
import SyncTypes
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath
import System.IO (SeekMode (AbsoluteSeek), hClose, hGetContents', hPutStr, hSeek, hTell)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.LibFuse3
import qualified System.LibFuse3.FuseConfig as FuseConfig
import System.LibFuse3.Internal (fuseRun)
import System.Posix (changeWorkingDirectoryFd, closeFd, dupTo, getEffectiveGroupID, getEffectiveUserID, openFd)
import System.Posix.Files (groupReadMode, otherReadMode, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO (OpenFileFlags (creat, nonBlock, trunc), OpenMode (..), defaultFileFlags, directory)
import System.Posix.Types (ByteCount, DeviceID, Fd (..), FileMode, FileOffset)
import Torrent
import TorrentFileSystem as TFS
import TorrentTypes (TorrentPieceSizeType)
import Utils (OptionalDebug (..), OptionalTrace (..), sortedUnionBy, unwrapFi, withTrace, (/%), (/.), (/^))

type FuseFDType = TFSHandle'

data DirFDType = DirHandle deriving (Show, Eq)

staticStateDir = Fd 512

torrentStateName = ".torrent_state"

fuseStateName = ".fuse_state"

defaultStates :: TChan SyncEvent -> Args.FuseTorrentArgs -> IO (FuseState, TorrentState)
defaultStates chan args = do
  let debug =
        if Args.debug args
          then Trace
          else NoTrace
  newFiles <- newTVarIO Set.empty
  traceM debug "Opening root"
  rootDir <- openFd (fromMaybe (Args.mountpoint args) $ Args.stateDir args) ReadOnly defaultFileFlags {directory = True}
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
  config <- newTVarIO Nothing
  lostFound <- if Args.lostAndFound args then Just <$> newTVarIO emptyFileSystem else return Nothing
  wLostFound <- forM lostFound (`mkWeakTVar` return ())
  return (FuseState {_files = ref, _syncChannel = chan, _hiddenDirs = [], _newFiles = newFiles, _realStatePath = (rootDir, fuseStateName), _lostFound = lostFound, _nonBlockTimeout = 0, _ignoreNonBlock = Args.ignoreNonBlock args, _cachedBlocks = Args.cachedBlocks args, _fuseTrace = debug, _fuseConfig = config}, TorrentState {_fuseFiles = weak, _statePath = torrentStateDir', _fuseLostFound = wLostFound, _torrentTrace = debug})

data FuseDied = FuseDied deriving (Show)

instance Exception FuseDied

main :: IO ()
main = do
  null' <- openFd "/dev/null" ReadOnly defaultFileFlags
  _ <- dupTo null' staticStateDir
  closeFd null'
  options <- Args.execParser Args.programInfo
  when (Args.justFuseRun options) $ do
    _ <- fuseRun "TorrentFuse" (Args.toFuseArgs options) defaultFuseOperations defaultExceptionHandler
    exitWith ExitSuccess
  let traceTo = withTrace $ Args.debug options
  traceShowM traceTo options
  traceShowM traceTo $ Args.toFuseArgs options
  (comChannel, comChannel') <- atomically $ do
    new <- newTChan
    second <- dupTChan new
    return (new, second)
  (fuseState, torrState) <- defaultStates comChannel options
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  let torrentMain = \config -> do
        atomically $ writeTVar (_fuseConfig fuseState) $ Just config {FuseConfig.uid = uid, FuseConfig.gid = gid, FuseConfig.useIno = False, FuseConfig.readdirIno = False}
        _ <- Sync.mainLoop comChannel' torrState
        pure config
  fuseRun "TorrentFuse" (Args.toFuseArgs options) (myFuseFSOps fuseState torrentMain) defaultExceptionHandler

myFuseFSOps :: FuseState -> (FuseConfig -> IO FuseConfig) -> FuseOperations FuseFDType DirFDType
myFuseFSOps state main' =
  defaultFuseOperations
    { fuseMknod = Just $ myFuseCreateDevice state,
      fuseMkdir = Just $ myFuseCreateDirectory state,
      fuseSymlink = Just $ myFuseCreateSymbolicLink state,
      fuseDestroy = Just $ myFuseDestroy state,
      fuseGetattr = Just $ myFuseGetFileStat state,
      fuseInit = Just main',
      fuseOpen = Just $ myFuseOpen state,
      fuseOpendir = Just $ myFuseOpenDirectory state,
      fuseRead = Just $ myFuseRead state,
      fuseReaddir = Just $ myFuseReadDirectory state,
      fuseReadlink = Just $ myFuseReadSymbolicLink state,
      fuseRelease = Just $ myFuseRelease state,
      fuseRmdir = Just $ myFuseRemoveDirectory state,
      fuseUnlink = Just $ myFuseRemoveLink state,
      fuseRename = Just $ myFuseRename state,
      fuseWrite = Just $ myFuseWrite state
    }

dummyDirStat ctx =
  defaultFileStat
    { fileMode =
        foldr1
          unionFileModes
          [ ownerReadMode,
            groupReadMode,
            otherReadMode,
            entryTypeToFileMode Directory
          ],
      linkCount = 1,
      fileOwner = FuseConfig.uid ctx,
      fileGroup = FuseConfig.gid ctx
    }

dummyFileStat ctx =
  defaultFileStat
    { fileMode =
        foldr1
          unionFileModes
          [ ownerReadMode,
            ownerWriteMode,
            groupReadMode,
            otherReadMode,
            entryTypeToFileMode RegularFile
          ],
      linkCount = 1,
      fileOwner = FuseConfig.uid ctx,
      fileGroup = FuseConfig.gid ctx
    }

fileStat :: FuseConfig -> TorrentFileSystemEntry' a -> FileStat
fileStat ctx a =
  defaultFileStat
    { fileMode =
        foldr1
          unionFileModes
          [ ownerReadMode,
            groupReadMode,
            otherReadMode,
            entryTypeToFileMode $ fileType a
          ],
      linkCount = 1,
      fileOwner = FuseConfig.uid ctx,
      fileGroup = FuseConfig.gid ctx,
      fileSize = fileSize',
      blockCount = (unwrapFi fileSize') /^ (unwrapFi $ blockSize a)
    }
  where
    fileType :: TorrentFileSystemEntry' a -> EntryType
    fileType x
      | TFSDir {} <- x = Directory
      | TFSFile {} <- x = RegularFile
      | TFSTorrentFile {} <- x = RegularFile
      | TFSUninitialized x' <- x,
        RegularFile <- fileType x' =
          Unknown
      | TFSLink {} <- x = SymbolicLink
      | TFSUninitialized x' <- x,
        x'' <- fileType x' =
          x''
    fileSize' = fileSize a
    fileSize :: TorrentFileSystemEntry' a -> FileOffset
    fileSize x
      | TFSDir {} <- x = 0
      | TFSLink {} <- x = 0
      | TFSFile {TFS._filesize = s} <- x = s
      | TFSTorrentFile {TFS._filesize = s} <- x = s
      | TFSUninitialized u <- x = fileSize u
    blockSize :: TorrentFileSystemEntry' a -> TorrentPieceSizeType
    blockSize x
      | TFSTorrentFile {TFS._pieceSize = ps} <- x = ps
      | otherwise = 512

applyMode :: FileMode -> FileStat -> FileStat
applyMode mode stat = stat {fileMode = unionFileModes mode $ fileMode stat}

applyOwnerWritable :: FileStat -> FileStat
applyOwnerWritable = applyMode ownerWriteMode

maybeLostWith :: (Monad m) => (TVar TorrentFileSystemEntryList'' -> m TorrentFileSystemEntryList'') -> FuseState -> FilePath -> m (FilePath, TorrentFileSystemEntryList'')
maybeLostWith readVar state path =
  let maybeLost
        | Just rest <- stripPrefix "lost+found/" path,
          Just lostFoundActive <- _lostFound state =
            Just (rest, readVar lostFoundActive)
        | otherwise = Nothing
   in case maybeLost of
        Just (rest, readLostFound) -> (rest,) <$> readLostFound
        Nothing -> (path,) <$> (readVar $ _files state)

maybeLost :: FuseState -> FilePath -> STM (FilePath, TorrentFileSystemEntryList'')
maybeLost = maybeLostWith readTVar

maybeLostIO :: FuseState -> FilePath -> IO (FilePath, TorrentFileSystemEntryList'')
maybeLostIO = maybeLostWith readTVarIO

getFuseConfig :: FuseState -> IO FuseConfig
getFuseConfig = ((pure . fromJust) <=< readTVarIO) . _fuseConfig

myFuseGetFileStat :: FuseState -> FilePath -> Maybe FuseFDType -> IO (Either Errno FileStat)
myFuseGetFileStat _ path _
  | DebugTrace.trace ("Getting file stat for " ++ show path) False = undefined
myFuseGetFileStat state "/" Nothing = Right . DebugTrace.traceShowId . dummyDirStat <$> getFuseConfig state
myFuseGetFileStat state "/lost+found" Nothing = Right . dummyDirStat <$> getFuseConfig state
myFuseGetFileStat state ('/' : path) Nothing = do
  ctx <- getFuseConfig state
  (path', files) <- maybeLostIO state path
  newFiles' <- readTVarIO $ _newFiles state
  let matching = getTFS' files path'
      reply
        | Just (x, _) <- matching = Right $ fileStat ctx x
        | Set.member path newFiles' = Right $ dummyFileStat ctx
        | otherwise = Left eNOENT
  return reply
myFuseGetFileStat state _ (Just TorrentFileHandle {_tfsEntry = e}) = do
  ctx <- getFuseConfig state
  pure $ Right $ fileStat ctx e
myFuseGetFileStat _ _ _ = error "Unknown entity"

-- return $ Left eNOENT

myFuseOpenDirectory :: FuseState -> FilePath -> IO (Either Errno DirFDType)
myFuseOpenDirectory _ path
  | DebugTrace.trace ("Opening directory " ++ show path) False = undefined
myFuseOpenDirectory _ path
  | DebugTrace.trace ("Opening dir " ++ show path) False = undefined
myFuseOpenDirectory _ "/" = return $ Right DirHandle
myFuseOpenDirectory _ "/lost+found" = return $ Right DirHandle
myFuseOpenDirectory state ('/' : path) = do
  (path', files) <- maybeLostIO state path
  let matching = getTFS' files path'
  return $ case matching of
    Just (TFSDir {}, _) -> Right DirHandle
    Just (TFSUninitialized (TFSDir {}), _) -> Right DirHandle
    _ -> Left eNOENT
myFuseOpenDirectory _ _ = return $ Left eNOENT

myFuseReadDirectory :: FuseState -> FilePath -> DirFDType -> IO (Either Errno [(FilePath, Maybe FileStat)])
myFuseReadDirectory _ path _
  | DebugTrace.trace ("Reading directory " ++ show path) False = undefined
myFuseReadDirectory state "/" _ = do
  ctx <- getFuseConfig state
  files <- readTVarIO $ _files state
  traceShowM state files
  -- newFiles <- readTVarIO $ _newFiles state
  lostFound <- mapM readTVarIO $ _lostFound state
  let lostFound' = if isJust lostFound then [("lost+found", Just $ dummyDirStat ctx)] else []
      builtin =
        [ (".", Just $ applyOwnerWritable $ dummyDirStat ctx),
          ("..", Just $ dummyDirStat ctx)
        ]
          ++ lostFound'
      directories = flip map (Map.toList files) $ \(name, file) ->
        ( name,
          Just $ fileStat ctx file
        )
  -- newFiles' = map (, applyOwnerWritable $ fileStat 0 Nothing ctx) $ Set.toList newFiles
  return $ Right $ traceShowId state $ builtin ++ directories --  ++ newFiles'
myFuseReadDirectory state@FuseState {_lostFound = Just lf} "/lost+found" _ = do
  ctx <- getFuseConfig state
  files <- readTVarIO lf
  traceShowM state files
  let builtin =
        [ (".", Just $ applyOwnerWritable $ dummyDirStat ctx),
          ("..", Just $ dummyDirStat ctx)
        ]
      -- TODO Handle files that has not been matched with a torrent
      directories = flip map (Map.toList files) $ \(name, file) ->
        ( name,
          Just $ fileStat ctx file
        )
  return $ Right $ traceShowId state $ builtin ++ directories
myFuseReadDirectory state ('/' : path) _ = do
  ctx <- getFuseConfig state
  (path', files') <- maybeLostIO state path
  traceShowM state files'

  let matching = getTFS files' path'
      builtin =
        [ (".", Just $ dummyDirStat ctx),
          ("..", Just $ dummyDirStat ctx)
        ]
  return $ Right $ traceShowId state $ builtin ++ map (\(name, e) -> (name, Just $ fileStat ctx e)) matching
myFuseReadDirectory _ _ _ = return $ Left eNOENT

myFuseOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FuseFDType)
myFuseOpen _ path _ _
  | DebugTrace.trace ("Opening file " ++ show path) False = undefined
myFuseOpen fuseState ('/' : path) ReadWrite _ = atomically $ do
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
myFuseOpen fuseState ('/' : path) ReadOnly flags = do
  (path', files) <- maybeLostIO fuseState path
  let matching = getTFS' files path'
  case matching of
    Just (fsEntry@TFSTorrentFile {TFS._torrent = torrent, TFS._pieceStart = pieceStart}, _) -> do
      uid <- liftIO $ newEmptyMVar
      atomically $ writeTChan (_syncChannel fuseState) $ OpenTorrent {SyncTypes._torrent = torrent, _fdCallback = uid, _piece = pieceStart}
      uid' <- takeMVar uid
      buffer <- newTVarIO []
      lastReq <- newTVarIO Nothing
      return $
        Right $
          TorrentFileHandle
            { _fileNoBlock = nonBlock flags,
              _tfsEntry = traceShowId fuseState $ fsEntry,
              _blockCache = buffer,
              _uid = uid',
              _lastRequest = lastReq
            }
    _ -> return $ Left eNOENT
myFuseOpen _ _ _ _ = return $ Left eNOENT

myFuseRename :: FuseState -> FilePath -> FilePath -> IO Errno
myFuseRename fuseState ('/' : from) ('/' : to)
  | Just _ <- _lostFound fuseState,
    Just _ <- inLostFound to =
      return eNOTSUP
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
  where
    myFuseRename' = do
      allFiles <- readTVar (_files fuseState)
      case myFuseRename'' $ takeTFS allFiles from of
        Left err -> return err
        Right files' -> do
          writeTVar (_files fuseState) files'
          return eOK
    inLostFound = stripPrefix "lost+found/"
    last [] = Nothing
    last [x] = Just x
    last (_ : x) = last x
    filename = last . splitDirectories
    myFuseRename'' :: Maybe (TorrentFileSystemEntry' HotTorrent, TorrentFileSystemEntryList'') -> Either Errno TorrentFileSystemEntryList''
    myFuseRename'' files
      | Just (f, files') <- files,
        Just newFile <- toTFSDir to f =
          Right $ mergeDirectories $ Map.toList files' ++ Map.toList newFile
      | Just (f, files') <- files,
        Just filename' <- filename from =
          Right $ mergeDirectories $ Map.toList files' ++ [(filename', f)]
      | Nothing <- files = Left eNOENT
      | otherwise = Left eINVAL
myFuseRename _ _ _ = return eNOENT

myFuseCreateDevice :: FuseState -> FilePath -> FileMode -> DeviceID -> IO Errno
myFuseCreateDevice state ('/' : path) _ _ =
  if takeExtension path == ".torrent"
    then (atomically $ modifyTVar (_newFiles state) (Set.insert path)) >> return eOK
    else return eACCES
myFuseCreateDevice _ _ _ _ = return eACCES

myFuseCreateDirectory :: FuseState -> FilePath -> FileMode -> IO Errno
myFuseCreateDirectory _ "/lost+found" _ = return eACCES
myFuseCreateDirectory fuseState ('/' : path) _ = do
  atomically $ modifyTVar (_files fuseState) $ \files ->
    let newDir = pathToTFSDir' path
     in (mergeDirectories2 files $ New newDir)
  return eOK
myFuseCreateDirectory _ _ _ = return eACCES

myFuseCreateSymbolicLink :: FuseState -> FilePath -> FilePath -> IO Errno
myFuseCreateSymbolicLink state target ('/' : path) = do
  atomically $ do
    files <- readTVar (_files state)
    let new = toTFSDir path $ uninitialized TFSLink {_target = target}
    case new of
      Just new' -> do
        if new' `fitsIn` files
          then do
            writeTVar (_files state) $ mergeDirectories2 files $ New new'
            return eOK
          else return eACCES
      Nothing -> return eINVAL
myFuseCreateSymbolicLink _ _ _ = return eNOENT

myFuseReadSymbolicLink :: FuseState -> FilePath -> IO (Either Errno FilePath)
myFuseReadSymbolicLink _ path
  | DebugTrace.trace ("Read link " ++ show path) False = undefined
myFuseReadSymbolicLink state ('/' : path) = do
  (path', files) <- maybeLostIO state path
  let matching = getTFS' files path'
      reply :: Maybe (TorrentFileSystemEntry' a, [FilePath]) -> (Either Errno FilePath)
      reply from
        | Just (TFSLink {_target = target}, _) <- from = Right target
        | Just (TFSUninitialized t, x) <- from = reply $ Just (t, x)
        | otherwise = Left eNOENT
  return $ reply matching
myFuseReadSymbolicLink _ _ = return $ Left eNOENT

myFuseRead :: FuseState -> FilePath -> FuseFDType -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
myFuseRead _ path _ _ _
  | DebugTrace.trace ("Read file " ++ show path) False = undefined
myFuseRead _ _ SimpleFileHandle {_fileHandle = fHandle} count offset = do
  pos <- hTell fHandle
  when (pos /= toInteger offset) $ hSeek fHandle AbsoluteSeek $ toInteger offset
  pos <- hTell fHandle
  if pos /= toInteger offset
    then pure (Left eNOENT) -- TODO: This is wrong
    else Right <$> B.hGet fHandle (fromJust $ toIntegralSized count)
myFuseRead _ _ (NewTorrentFileHandle _ buffer) count offset = Right . B.take (fromJust $ toIntegralSized count) . B.drop (fromJust $ toIntegralSized offset) <$> readTVarIO buffer
myFuseRead fuseState _ TorrentFileHandle {_tfsEntry = TFSTorrentFile {TFS._torrent = torrHandle, TFS._pieceStart = pieceStart, TFS._pieceSize = pieceSize, TFS._pieceStartOffset = pieceStartOffset, TFS._filesize = filesize}, _blockCache = blockCache, _uid = fd, _fileNoBlock = fileNoBlock, _lastRequest = prev} count offset = do
  directIo <- FuseConfig.directIo <$> getFuseConfig fuseState
  lastRequest' <- atomically $ swapTVar prev $ Just offset

  let blockCacheSize = unwrapFi $ _cachedBlocks fuseState
      count'
        | filesize < offset = 0
        | otherwise = max 0 (min (filesize - offset) $ unwrapFi count)
      (firstPiece, pieceOffset') = (\(q, m) -> (q + pieceStart, m)) $ (unwrapFi offset + pieceStartOffset) /% pieceSize
      (piecesCount, _) = (unwrapFi count' + pieceOffset') /% pieceSize
      currentReadLastPiece = firstPiece + piecesCount
      _fileLastPiece = (filesize /. unwrapFi pieceSize) - unwrapFi (pieceStartOffset /^ pieceSize)
      wantedPieces
        | count' == 0 = []
        | otherwise = [firstPiece .. currentReadLastPiece]
  traceM fuseState $ "Want pieces " ++ show wantedPieces

  blockCache' <- atomically $ readTVar blockCache
  let takeMissing hasPiece wantsPiece
        | (p1, _) : (p2, _) : _ <- hasPiece,
          p1 >= p2 =
            error "hasPiece aren't sorted and unique"
        | p1 : p2 : _ <- wantsPiece,
          p1 >= p2 =
            error "wantsPiece aren't sorted and unique"
        | (p1, _) : hx <- hasPiece,
          p2 : wx <- wantsPiece,
          fromIntegral p1 == fromIntegral p2 =
            takeMissing hx wx
        | (p1, _) : hx <- hasPiece,
          wx@(p2 : _) <- wantsPiece,
          p2 > p1 =
            takeMissing hx wx
        | wanted : wx <- wantsPiece = wanted : takeMissing hasPiece wx
        | otherwise = []
      requestPieces = takeMissing blockCache' wantedPieces
      fileNoBlock'
        | Just fo <- lastRequest',
          fo == offset =
            False
        | _ignoreNonBlock fuseState = False
        | otherwise = fileNoBlock
      filterWanted _ [] = Just []
      filterWanted [] _ = Just []
      filterWanted ((piece, d) : next_blocks) all_wanted@(wanted : next_wanted)
        | piece == wanted = fmap (d :) $ filterWanted next_blocks next_wanted
        | piece < wanted = filterWanted next_blocks all_wanted
        | otherwise = DebugTrace.trace "Could not find wanted block" Nothing
      readData blocks
        | fileNoBlock' = do
            atomically $ do
              let readBlocks [] = pure $ Right []
                  readBlocks (b : bx) = do
                    blockData' <- tryReadTMVar b
                    case blockData' of
                      Nothing -> pure $ Left eWOULDBLOCK
                      Just d -> do
                        rest <- readBlocks bx
                        case rest of
                          Right dx -> pure $ Right (d : dx)
                          Left err -> pure $ Left err
              b <- readBlocks blocks
              case b of
                Right b -> pure $ Right $ LBS.fromChunks b
                Left err -> pure $ Left err
        | otherwise = do
            chunks <- flip mapM blocks $ \b -> unsafeInterleaveIO $ atomically $ readTMVar b
            pure $ Right $ LBS.fromChunks chunks
  requests <- mapM (\p -> (p,) <$> newTorrentReadCallback) requestPieces
  traceM fuseState $ "Has pieces " ++ show (fmap fst blockCache')
  traceM fuseState $ "Want additional " ++ show (fmap fst requests)
  let sequentialReqs r
        | r' : rx <- r,
          t@ReadTorrent {} <- thisReq r',
          rest@(next : restx) <- sequentialReqs rx =
            if _piece t + 1 == _piece next
              then t {_pieceData = _pieceData t ++ _pieceData next} : restx
              else t : rest
        | [] <- r = []
        | r' : rx <- r = thisReq r' : sequentialReqs rx
        where
          thisReq (n, (_, fetch)) =
            ReadTorrent
              { SyncTypes._torrent = torrHandle,
                _fd = fd,
                _piece = unwrapFi n,
                _pieceData = [fetch]
              }
      reqs = sequentialReqs requests
  _ <- forM reqs $ atomically . writeTChan (_syncChannel fuseState)
  let newCache = fmap (\(piece, (d, _)) -> (piece, d)) requests
      dataCache = sortedUnionBy fst blockCache' newCache
      shouldSave (x, _)
        | x + blockCacheSize < firstPiece = False
        | x > currentReadLastPiece + blockCacheSize = False
        | otherwise = True
  atomically $ writeTVar blockCache $ filter shouldSave dataCache
  let replyBlocks = dataCache `filterWanted` wantedPieces
  replData <- mapM readData $ if directIo then fmap (take 1) replyBlocks else replyBlocks
  traceM fuseState $ "New cache contains " ++ (show $ fmap fst dataCache)
  traceM fuseState $ "Reading " ++ (if directIo then "direct " else "") ++ (if fileNoBlock' then "" else "blocking ") ++ show wantedPieces ++ " for offset " ++ show offset ++ " with first piece " ++ show firstPiece ++ " and piece offset " ++ show pieceOffset'
  let cutData = case replData of
        Just (Right b) -> Right $ LBS.toStrict $ LBS.take (unwrapFi count') $ LBS.drop (unwrapFi pieceOffset') b
        Just (Left x) -> Left x
        Nothing -> Left eNOENT
      dataLen = case cutData of
        Right b -> Just $ BS.length b
        _ -> Nothing

  DebugTrace.traceShowM $ "Dropping " ++ show pieceOffset' ++ " and taking " ++ show count' ++ " bytes. Result is " ++ show dataLen
  return cutData
myFuseRead _ _ _ _ _ = return $ Left eBADF

myFuseWrite :: FuseState -> FilePath -> FuseFDType -> B.ByteString -> FileOffset -> IO (Either Errno CInt)
myFuseWrite _ _ (NewTorrentFileHandle _ buffer) input offset = atomically $ do
  buffer' <- readTVar buffer
  if Just offset == toIntegralSized (B.length buffer')
    then do
      writeTVar buffer $ B.append buffer' input
      return $ Right $ fromJust $ toIntegralSized $ B.length input
    else return $ Left eWOULDBLOCK
myFuseWrite _ _ _ _ _ = return $ Left eBADF

myFuseRemoveDirectory :: FuseState -> FilePath -> IO Errno
myFuseRemoveDirectory fuseState ('/' : path) = atomically $ do
  files' <- readTVar $ _files fuseState
  case takeTFS files' path of
    (Just (TFSDir {_contents = c}, n)) ->
      if null c
        then do
          writeTVar (_files fuseState) n
          return eOK
        else return eNOTEMPTY
    _ -> return eNOTSUP
myFuseRemoveDirectory _ _ = return eNOENT

myFuseRemoveLink :: FuseState -> FilePath -> IO Errno
myFuseRemoveLink fuseState ('/' : path) = do
  action <- atomically $ do
    files' <- readTVar $ _files fuseState
    let sendRemove torrent = traceShowM fuseState ("Requesting removal of torrent", torrent) >> writeTChan (_syncChannel fuseState) (RemoveTorrent torrent) >> return eOK
        file' from
          | Just (f, n) <- from,
            Just hot <- assertHotBackend f =
              Just (hot, n)
          | otherwise = from
    case file' $ takeTFS files' path of
      Just (TFSTorrentFile {TFS._torrent = torrent, _singleFileTorrent = True}, _) -> return $ sendRemove torrent
      Just (TFSTorrentFile {TFS._torrent = t, TFS._hash = h}, fs) -> do
        let filter' = \case
              TFSTorrentFile {_hash = h'} -> h' == h
              TFSUninitialized (TFSTorrentFile {_hash = h'}) -> h' == h
              _ -> False
            lastFile = isNothing $ filterFS filter' $ Map.toList fs
        writeTVar (_files fuseState) fs
        if lastFile
          then return $ sendRemove t
          else return $ return eOK
      Just (TFSUninitialized (TFSTorrentFile {}), fs) -> do
        writeTVar (_files fuseState) fs
        return $ return eOK
      Just (TFSLink {}, fs) -> do
        writeTVar (_files fuseState) fs
        return $ return eOK
      _ -> return $ return eNOENT
  atomically action
myFuseRemoveLink _ _ = return eNOENT

myFuseRelease :: FuseState -> FilePath -> FuseFDType -> IO ()
myFuseRelease _ path _
  | DebugTrace.trace ("Relase " ++ show path) False = undefined
myFuseRelease _ _ SimpleFileHandle {_fileHandle = fh} = hClose fh
myFuseRelease fuseState _ TorrentFileHandle {_uid = uid, _tfsEntry = TFSTorrentFile {TFS._torrent = torrent}} = atomically $ writeTChan (_syncChannel fuseState) $ CloseTorrent {SyncTypes._torrent = torrent, _fd = uid}
myFuseRelease _ _ TorrentFileHandle {_tfsEntry = _} = return ()
myFuseRelease fuseState _ (NewTorrentFileHandle path content) = do
  content' <- readTVarIO content
  traceShowM fuseState ("Torrent closed with size", BS.length content') -- was 479376 for ubuntu
  atomically $ writeTChan (_syncChannel fuseState) $ AddTorrent (Just $ takeDirectory path) $ NewTorrentFile content'

myFuseDestroy :: FuseState -> IO ()
myFuseDestroy _
  | DebugTrace.trace ("Destroy") False = undefined
myFuseDestroy fuseState = do
  torrentDead <- newEmptyMVar
  atomically $ writeTChan (_syncChannel fuseState) $ FuseDead torrentDead
  let (fd, state) = _realStatePath fuseState
  files <- readTVarIO $ _files fuseState
  flip catchIOError (traceShowM fuseState) $
    withFileAt fd state WriteOnly defaultFileFlags {creat = Just 0o700, trunc = True} $
      \f -> hPutStr f $ show $ intoStoredTorrent files
  void $ takeMVar torrentDead
  traceM fuseState "Torrent is dead, finalizing"
