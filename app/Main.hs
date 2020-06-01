{-# LANGUAGE LambdaCase #-}
module Main where

import Torrent
import TorrentFileSystem
import qualified Data.ByteString.Char8 as B
import Control.Exception
import System.Exit (exitWith, ExitCode(..))
import System.Mem.Weak (Weak, deRefWeak)
import System.Environment (getEnvironment)
import Data.List (find)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import Foreign.C.Error
import System.Posix.Types
import GHC.IO.Handle (hDuplicateTo)
import Control.Monad (void)
import System.Posix.Files
import Control.Concurrent
import Data.IORef
import Control.Concurrent.Chan (newChan, writeChan)
import System.FilePath
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.ExecutionStack (showStackTrace)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
import qualified Sync
import Sync hiding (mainLoop)
-- import System.Posix.IO

import System.Fuse

import Debug.Trace

defaultStates :: IO (FuseState, TorrentState)
defaultStates = do
  ref <- newIORef []
  weak <- mkWeakIORef ref $ return ()
  return (FuseState { fuseFiles = ref }, TorrentState { torrentFiles = weak })

type HT = ()

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

helloFSOps :: FuseState -> IO () -> FuseOperations HT
helloFSOps state main = defaultFuseOps { fuseGetFileStat = helloGetFileStat state
                            , fuseOpen        = helloOpen state
                            , fuseRead        = helloRead state
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
                                 , statFileSize = fromIntegral $ filesize
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
             (f:_) -> if isJust $ tfsContent f
                         then Right $ dirStat ctx
                         else Right $ fileStat ctx $ tfsFilesize f
             _ -> Left eNOENT

helloGetFileStat state _ =
    return $ Left eNOENT

helloOpenDirectory :: FuseState -> FilePath -> IO Errno
helloOpenDirectory _ "/" = return eOK
helloOpenDirectory state ('/':path) = do
  files <- readIORef $ fuseFiles state
  let matching = getTFS files path
  withFile "/tmp/fuse.log" AppendMode $ \handle -> do
    hPrint handle ("open", path, matching)
  return $ if isJust $ find (isJust . tfsContent) matching
              then eOK
              else eNOENT


helloReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readIORef $ fuseFiles state
    let builtin = [(".",          dirStat  ctx)
                  ,("..",         dirStat  ctx)]
        directories = flip mapMaybe files $ \case
                                               TFSTorrentDir _ name _ -> Just (name, dirStat ctx)
                                               TFSDir name _ -> Just (name, dirStat ctx)
                                               TFSFile name size -> Just (name, fileStat ctx size)
                                               TFSTorrentFile _ name -> Just (name, fileStat ctx 0)
    withFile "/tmp/fuse.log" AppendMode $ \handle -> do
      hPrint handle ("read", builtin, directories)
    return $ Right $ builtin ++ directories
helloReadDirectory state ('/':path) = do
  ctx <- getFuseContext
  files <- readIORef $ fuseFiles state

  let matching = filter (isJust . tfsContent) $ getTFS files path
      allMatching = concatMap (fromJust . tfsContent) matching
      builtin = [(".",          dirStat  ctx)
                ,("..",         dirStat  ctx)] 
  withFile "/tmp/fuse.log" AppendMode $ \handle -> do
    hPrint handle ("read2", path, allMatching)
  return $ Right $ builtin ++ map (\t -> let (_, name, content) = tfsMetaData t
                                           in (name, (if isJust content then dirStat else flip fileStat (tfsFilesize t)) ctx)) allMatching

helloOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen fuseState path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | path == hello2Path = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


helloRead :: FuseState -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead fuseState a b c d = do
  myId <- myThreadId
  doLog $ "Got request at thread " ++ show myId ++ " for file " ++ show b ++ ", waiting for 10 seconds"
  threadDelay 10000000
  doLog $ "Wait for " ++ show myId ++ " completed"
  ret <- helloRead2 a b c d
  doLog $ "Returning for thread " ++ show myId
  return ret
  --let readCounter = fuseState
  --counter <- atomicModifyIORef readCounter $ \before -> (before+1,before)
  --if mod counter 3 == 0
  --  then helloRead2 a b c d
  --  else do
  --    return $ Left eAGAIN

helloRead2 :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead2 path _ byteCount offset
    | path == helloPath =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
    | path == hello2Path =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
    | otherwise         = return $ Left eNOENT

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
