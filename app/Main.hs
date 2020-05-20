module Main where

import Lib
import Torrent
import qualified Data.ByteString.Char8 as B
import Control.Exception
import System.Exit (exitWith, ExitCode(..))
import System.Mem.Weak (Weak, deRefWeak)
import System.Environment (getEnvironment)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import Control.Concurrent
import Data.IORef
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.ExecutionStack (showStackTrace)
import System.IO (withFile, IOMode(AppendMode), hPutStrLn, IOMode(WriteMode), hFlush)
-- import System.Posix.IO

import System.Fuse

data TorrentFileSystemEntry = TFSTorrentFile TorrentHandle String FileOffset
                            | TFSTorrentDir TorrentHandle String TorrentFileSystemEntry
                            | TFSFile String
                            | TFSDir String TorrentFileSystemEntry
                            | TFSEmpty
                            deriving Show

data FuseState = FuseState { fuseFiles :: IORef [TorrentFileSystemEntry] }
data TorrentState = TorrentState { torrentFiles :: Weak (IORef [TorrentFileSystemEntry]) }

defaultStates :: IO (FuseState, TorrentState)
defaultStates = do
  ref <- newIORef [TFSEmpty]
  weak <- mkWeakIORef ref $ return ()
  return $ (FuseState { fuseFiles = ref }, TorrentState { torrentFiles = weak })

type HT = ()

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  env <- getEnvironment
  (fuseState, torrState) <- defaultStates
  let torrentMain = withTorrentSession "/tmp/torrent_session.sess" $ \sess -> withFile "/tmp/torrent.log" WriteMode $ \log -> do
        torrent <- maybe (return Nothing) (\(_, t) -> addTorrent sess t "/tmp/torrent_test" >>= return . Just) (find (((==)"TEST_TORRENT") . fst) env)
        hPutStrLn log $ show sess
        hPutStrLn log $ show torrent
        let mainLoop = do
              a <- waitForAlert sess 1000
              hPutStrLn log $ show a
              hFlush log
              let getTorrentInfo torrent = do
                    name <- getTorrentName sess torrent
                    files <- getTorrentFiles sess torrent
                    return $ Just (name, files)
              outputNew <- maybe (return Nothing) getTorrentInfo torrent
              Just ref <- deRefWeak $ torrentFiles torrState
              maybe (return ()) (\(name, _) -> maybe (return ()) (\name -> writeIORef ref [TFSDir name TFSEmpty]) name) outputNew
              mainLoop
        hPutStrLn log "Before mainLoop"
        mainLoop
  fuseMain (helloFSOps fuseState torrentMain) defaultExceptionHandler

doLog str = return () -- withFile "/home/marcus/Projects/fuse.torrent/debug.log" AppendMode $ flip hPutStrLn str

helloFSOps :: FuseState -> IO () -> FuseOperations HT
helloFSOps state main = defaultFuseOps { fuseGetFileStat = helloGetFileStat state
                            , fuseOpen        = helloOpen state
                            , fuseRead        = helloRead state
                            , fuseOpenDirectory = helloOpenDirectory state
                            , fuseReadDirectory = helloReadDirectory state
                            , fuseGetFileSystemStats = helloGetFileSystemStats state
                            , fuseInit = forkIO main >> return ()
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
                                 , statFileSize = fromIntegral $ B.length helloString
                                 , statBlocks = 1
                                 , statAccessTime = 0
                                 , statModificationTime = 0
                                 , statStatusChangeTime = 0
                                 }

helloGetFileStat :: FuseState -> FilePath -> IO (Either Errno FileStat)
helloGetFileStat state "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
helloGetFileStat state path
  | path == helloPath = do
      ctx <- getFuseContext
      return $ Right $ fileStat ctx $ B.length helloString
  | path == hello2Path = do
      ctx <- getFuseContext
      return $ Right $ fileStat ctx $ B.length helloString
helloGetFileStat state _ =
    return $ Left eNOENT

helloOpenDirectory :: FuseState -> FilePath -> IO Errno
helloOpenDirectory _ "/" = return eOK
helloOpenDirectory _ _   = return eNOENT

helloReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readIORef $ fuseFiles state
    let builtin = [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)]
        directories = flip mapMaybe files $ \f -> case f of
                                                    TFSTorrentDir _ name _ -> Just (name, dirStat ctx)
                                                    TFSDir name _ -> Just (name, dirStat ctx)
                                                    _ -> Nothing
    putStrLn $ show $ builtin ++ directories
    return $ Right $ builtin ++ directories
helloReadDirectory state _ = return (Left (eNOENT))

helloOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen fuseState path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> do
                              return (Right ())
                            _        -> return (Left eACCES)
    | path == hello2Path = case mode of
                            ReadOnly -> do
                              return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


helloRead :: FuseState -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead fuseState a b c d = do
  myId <- myThreadId
  doLog $ "Got request at thread " ++ (show myId) ++ " for file " ++ (show b) ++ ", waiting for 10 seconds"
  threadDelay 10000000
  doLog $ "Wait for " ++ (show myId) ++ " completed"
  ret <- helloRead2 a b c d
  doLog $ "Returning for thread " ++ (show myId)
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
