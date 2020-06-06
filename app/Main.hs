{-# LANGUAGE LambdaCase #-}
module Main where

import Torrent
import TorrentFileSystem
import TorrentTypes (SyncEvent(..))
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

type FuseFDType = (TFSHandle, TorrentFileSystemEntry)

defaultStates :: IO (FuseState, TorrentState)
defaultStates = do
  ref <- newIORef []
  weak <- mkWeakIORef ref $ return ()
  return (FuseState { fuseFiles = ref }, TorrentState { files = weak })

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  env <- getEnvironment
  (fuseState, torrState) <- defaultStates
  comChannel <- newChan
  let torrentMain = do
        case find ((==)"TEST_TORRENT" . fst) env of
          Just (_, torr) -> writeChan comChannel $ AddTorrent torr "/tmp/torrent_test"
          Nothing -> return ()
        -- torrent <- maybe (return Nothing) (\(_, t) -> Just <$> addTorrent sess t "/tmp/torrent_test") 
        -- hPrint log torrent
        Sync.mainLoop comChannel torrState
        return ()
  fuseMain (helloFSOps fuseState torrentMain) defaultExceptionHandler

doLog str = return () -- withFile "/home/marcus/Projects/fuse.torrent/debug.log" AppendMode $ flip hPutStrLn str

helloFSOps :: FuseState -> IO () -> FuseOperations FuseFDType
helloFSOps state main = defaultFuseOps { fuseGetFileStat = helloGetFileStat state
                            , fuseOpen        = helloOpen state
                            , fuseRead        = helloRead state
                            , fuseRelease     = helloRelease state
                            , fuseOpenDirectory = helloOpenDirectory state
                            , fuseReadDirectory = helloReadDirectory state
                            , fuseGetFileSystemStats = helloGetFileSystemStats state
                            , fuseInit = void main
                            }
helloString :: B.ByteString
helloString = B.pack $ concat $ replicate 40 "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"
hello2Path :: FilePath
hello2Path = "/hello2"
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

fileStat ctx filesize = FileStat { statEntryType = RegularFile
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
                                 , statBlocks = 1
                                 , statAccessTime = 0
                                 , statModificationTime = 0
                                 , statStatusChangeTime = 0
                                 }

helloGetFileStat :: FuseState -> FilePath -> IO (Either Errno FileStat)
helloGetFileStat state "/" = Right . dirStat <$> getFuseContext
helloGetFileStat state ('/':path) = do
  ctx <- getFuseContext
  files <- readIORef $ fuseFiles state
  let matching = getTFS files path
  return $ case matching of
             (f:_) -> if isJust $ f^?contents
                         then Right $ dirStat ctx
                         else Right $ fileStat ctx $ f^?!filesize
             _ -> Left eNOENT

helloGetFileStat state _ =
    return $ Left eNOENT

helloOpenDirectory :: FuseState -> FilePath -> IO Errno
helloOpenDirectory _ "/" = return eOK
helloOpenDirectory state ('/':path) = do
  files <- readIORef $ fuseFiles state
  let matching = getTFS files path
  withFile "/tmp/fuse.log" AppendMode $ \handle ->
    hPrint handle ("open", path, matching)
  return $ if isJust $ find (isJust . (^?contents)) matching
              then eOK
              else eNOENT


helloReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readIORef $ fuseFiles state
    let builtin = [(".",          dirStat  ctx)
                  ,("..",         dirStat  ctx)]
        directories = flip mapMaybe files $ \file -> Just (file^.name, case file^?filesize of
                                                                         Just size -> fileStat ctx size
                                                                         Nothing -> dirStat ctx)
    withFile "/tmp/fuse.log" AppendMode $ \handle ->
      hPrint handle ("read", builtin, directories)
    return $ Right $ builtin ++ directories
helloReadDirectory state ('/':path) = do
  ctx <- getFuseContext
  files <- readIORef $ fuseFiles state

  let matching = filter (isJust . (^?contents)) $ getTFS files path
      allMatching = concatMap (^?!contents) matching
      builtin = [(".",          dirStat  ctx)
                ,("..",         dirStat  ctx)] 
  withFile "/tmp/fuse.log" AppendMode $ \handle ->
    hPrint handle ("read2", path, allMatching)
  return $ Right $ builtin ++ map (\t -> (t^.name, (if isJust (t^?contents) then dirStat else flip fileStat $ t^?!filesize) ctx)) allMatching

helloOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FuseFDType)
helloOpen fuseState ('/':path) mode flags = do
    files <- readIORef $ fuseFiles fuseState
    let matching = getTFS files path
    withFile "/tmp/fuse.log" AppendMode $ \handle ->
      hPrint handle ("open", path, matching)
    case matching of
      [fsEntry@TFSTorrentFile{}] -> Right . (\x -> (x, fsEntry)) . TorrentFileHandle <$> openBinaryFile (fsEntry^.realPath) ReadMode
      _ -> return $ Left eNOENT


helloRead :: FuseState -> FilePath -> FuseFDType -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead fuseState _ (handle@SimpleFileHandle{}, entry) count offset = do
  pos <- hTell $ handle^.fileHandle
  when (pos /= fromIntegral offset) $ hSeek (handle^.fileHandle) AbsoluteSeek $ fromIntegral offset
  Right <$> B.hGet (handle^.fileHandle) (fromIntegral count)

helloRead fuseState _ (handle@TorrentFileHandle{}, entry) count offset = do
  pos <- hTell $ handle^.fileHandle
  when (pos /= fromIntegral offset) $ hSeek (handle^.fileHandle) AbsoluteSeek $ fromIntegral offset
  Right <$> B.hGet (handle^.fileHandle) (fromIntegral count)

helloRelease :: FuseState -> FilePath -> FuseFDType -> IO ()
helloRelease _ _ (fh, _) = hClose $ fh^.fileHandle

helloGetFileSystemStats :: FuseState -> String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats _ str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
