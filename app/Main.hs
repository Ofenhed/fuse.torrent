module Main where

import Torrent
import TorrentFileSystem
import SyncTypes
import qualified Sync

import Control.Concurrent
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Exception
import Control.Lens
import Control.Monad (void, when)
import Data.IORef
import Data.List (find)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import Foreign.C.Error
import GHC.ExecutionStack (showStackTrace)
import GHC.IO.Handle (hDuplicateTo)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnvironment)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.Fuse
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode, ReadMode), hFlush, stderr, stdout, Handle, openBinaryFile, hClose, hTell, hSeek, SeekMode(AbsoluteSeek))
import System.Mem.Weak (Weak, deRefWeak)
import System.Posix.Files
import System.Posix.Types
import Sync hiding (mainLoop)

import qualified Data.ByteString.Char8 as B


import Debug.Trace

type FuseFDType = TFSHandle

quotCeil x y = quot x y + if rem x y /= 0 then 1 else 0

defaultStates :: Chan SyncEvent -> IO (FuseState, TorrentState)
defaultStates chan = do
  ref <- newIORef []
  weak <- mkWeakIORef ref $ return ()
  return (FuseState { _files = ref, _syncChannel = chan }, TorrentState { _fuseFiles = weak })

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  env <- getEnvironment
  comChannel <- newChan
  (fuseState, torrState) <- defaultStates comChannel
  let torrentMain = do
        case find ((==)"TEST_TORRENT" . fst) env of
          Just (_, torr) -> writeChan comChannel $ AddTorrent torr "/tmp/torrent_test"
          Nothing -> return ()
        -- torrent <- maybe (return Nothing) (\(_, t) -> Just <$> addTorrent sess t "/tmp/torrent_test")
        -- hPrint log torrent
        Sync.mainLoop comChannel torrState
        return ()
  fuseMain (myFuseFSOps fuseState torrentMain) defaultExceptionHandler

doLog str = return () -- withFile "/home/marcus/Projects/fuse.torrent/debug.log" AppendMode $ flip hPutStrLn str

myFuseFSOps :: FuseState -> IO () -> FuseOperations FuseFDType
myFuseFSOps state main = defaultFuseOps { fuseGetFileStat = myFuseGetFileStat state
                            , fuseOpen        = myFuseOpen state
                            , fuseRead        = myFuseRead state
                            , fuseRelease     = myFuseRelease state
                            , fuseOpenDirectory = myFuseOpenDirectory state
                            , fuseReadDirectory = myFuseReadDirectory state
                            , fuseGetFileSystemStats = myFuseGetFileSystemStats state
                            , fuseInit = void main
                            }
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
                                           , statFileSize = fromIntegral filesize
                                           , statBlocks = quotCeil (fromIntegral filesize) (maybe 1 fromIntegral blocksize)
                                           , statAccessTime = 0
                                           , statModificationTime = 0
                                           , statStatusChangeTime = 0
                                           }

myFuseGetFileStat :: FuseState -> FilePath -> IO (Either Errno FileStat)
myFuseGetFileStat state "/" = Right . dirStat <$> getFuseContext
myFuseGetFileStat state ('/':path) = do
  ctx <- getFuseContext
  files <- readIORef $ state^.files
  let matching = getTFS files path
  return $ case matching of
             (f:_) -> if isJust $ f^?contents
                         then Right $ dirStat ctx
                         else Right $ fileStat (f^?!filesize) (Just $ f^?!pieceSize) ctx
             _ -> Left eNOENT

myFuseGetFileStat state _ =
    return $ Left eNOENT

myFuseOpenDirectory :: FuseState -> FilePath -> IO Errno
myFuseOpenDirectory _ "/" = return eOK
myFuseOpenDirectory state ('/':path) = do
  files <- readIORef $ state^.files
  let matching = getTFS files path
  withFile "/tmp/fuse.log" AppendMode $ \handle ->
    hPrint handle ("open", path, matching)
  return $ if isJust $ find (isJust . (^?contents)) matching
              then eOK
              else eNOENT


myFuseReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
myFuseReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readIORef $ state^.files
    let builtin = [(".",          dirStat  ctx)
                  ,("..",         dirStat  ctx)]
        directories = flip mapMaybe files $ \file -> Just (file^.name, case file^?filesize of
                                                                         Just size -> fileStat size (file^?pieceSize) ctx
                                                                         Nothing -> dirStat ctx)
    withFile "/tmp/fuse.log" AppendMode $ \handle ->
      hPrint handle ("read", builtin, directories)
    return $ Right $ builtin ++ directories
myFuseReadDirectory state ('/':path) = do
  ctx <- getFuseContext
  files <- readIORef $ state^.files

  let matching = filter (isJust . (^?contents)) $ getTFS files path
      allMatching = concatMap (^?!contents) matching
      builtin = [(".",          dirStat  ctx)
                ,("..",         dirStat  ctx)]
  withFile "/tmp/fuse.log" AppendMode $ \handle ->
    hPrint handle ("read2", path, allMatching)
  return $ Right $ builtin ++ map (\t -> (t^.name, (if isJust (t^?contents) then dirStat else fileStat (t^?!filesize) (t^?pieceSize)) ctx)) allMatching

myFuseOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FuseFDType)
myFuseOpen fuseState ('/':path) mode flags = do
    files <- readIORef $ fuseState^.files
    let matching = getTFS files path
    withFile "/tmp/fuse.log" AppendMode $ \handle ->
      hPrint handle ("open", path, matching)
    case matching of
      [fsEntry@TFSTorrentFile{}] -> Right . (`TorrentFileHandle` fsEntry) <$> openBinaryFile (fsEntry^.realPath) ReadMode
      _ -> return $ Left eNOENT


myFuseRead :: FuseState -> FilePath -> FuseFDType -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
myFuseRead fuseState _ handle count offset = do
  pos <- hTell $ handle^.fileHandle
  when (pos /= fromIntegral offset) $ hSeek (handle^.fileHandle) AbsoluteSeek $ fromIntegral offset
  withFile "/tmp/fuse.log" AppendMode $ \handle' ->
    hPrint handle' ("read", count, offset, handle)
  case handle of
    SimpleFileHandle{} -> Right <$> B.hGet (handle^.fileHandle) (fromIntegral count)
    TorrentFileHandle{} -> do
      sem <- newQSem 0
      let tfs = handle^?!tfsEntry
          offset' = fromIntegral offset
          count' = fromIntegral count
          pieceStart' = fromIntegral $ tfs^?!pieceStart
          pieceStartOffset' = fromIntegral $ tfs^?!pieceStartOffset
          pieceSize' = fromIntegral $ tfs^?!pieceSize
          piece' = quot (offset' + pieceStartOffset') pieceSize' + pieceStart'
          req = RequestFileContent { SyncTypes._torrent = tfs^.TorrentFileSystem.torrent
                                   , _piece = piece'
                                   , _count = quotCeil count' pieceSize'
                                   , _callback = sem }
          chan = fuseState^.syncChannel
      withFile "/tmp/fuse.log" AppendMode $ \handle ->
        hPrint handle ("sync", offset, req^?piece, req^?SyncTypes.count)
      writeChan chan req
      waitQSem sem
      result <- Right <$> B.hGet (handle^.fileHandle) (fromIntegral count)
      withFile "/tmp/fuse.log" AppendMode $ \handle ->
        hPrint handle ("sync2", offset, either (const "errno") (\x -> show (B.length x, x)) result)
      return result



myFuseRelease :: FuseState -> FilePath -> FuseFDType -> IO ()
myFuseRelease _ _ fh = hClose $ fh^.fileHandle

myFuseGetFileSystemStats :: FuseState -> String -> IO (Either Errno FileSystemStats)
myFuseGetFileSystemStats _ str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
