module Main where

import Lib
import Torrent
import qualified Data.ByteString.Char8 as B
import Control.Exception
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getEnvironment)
import Data.List (find)
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import Control.Concurrent
import Data.IORef
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.ExecutionStack (showStackTrace)
import System.IO (withFile, IOMode(AppendMode), hPutStrLn, IOMode(WriteMode))
-- import System.Posix.IO

import System.Fuse

data FuseState = FuseState deriving Show

fuseState :: IORef Int
fuseState = unsafePerformIO (newIORef 0)

type HT = Int

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  env <- getEnvironment
  let Just (_, testTorrent) = find (\(key, _) -> key == "TEST_TORRENT") env
  putStrLn testTorrent
  let torrentThread = forkIO $ withTorrentSession $ \sess -> withFile "/tmp/torrent.log" WriteMode $ \log -> do
        addTorrent sess testTorrent "/tmp/torrent_test"
        hPutStrLn log $ show sess
        let mainLoop = do
              threadDelay 1000000
              mainLoop
        hPutStrLn log "Before mainLoop"
        mainLoop
  fuseMain (helloFSOps { fuseInit = torrentThread >> return () }) defaultExceptionHandler

doLog str = return () -- withFile "/home/marcus/Projects/fuse.torrent/debug.log" AppendMode $ flip hPutStrLn str

helloFSOps :: FuseOperations HT
helloFSOps = defaultFuseOps { fuseGetFileStat = helloGetFileStat
                            , fuseOpen        = helloOpen
                            , fuseRead        = helloRead 
                            , fuseOpenDirectory = helloOpenDirectory
                            , fuseReadDirectory = helloReadDirectory
                            , fuseGetFileSystemStats = helloGetFileSystemStats
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

fileStat ctx = FileStat { statEntryType = RegularFile
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

helloGetFileStat :: FilePath -> IO (Either Errno FileStat)
helloGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
helloGetFileStat path
  | path == helloPath = do
      ctx <- getFuseContext
      return $ Right $ fileStat ctx
  | path == hello2Path = do
      ctx <- getFuseContext
      return $ Right $ fileStat ctx
helloGetFileStat _ =
    return $ Left eNOENT

helloOpenDirectory "/" = return eOK
helloOpenDirectory _   = return eNOENT

helloReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(helloName,    fileStat ctx)
                   ,(hello2Name,    fileStat ctx)
                   ]
    where (_:helloName) = helloPath
          (_:hello2Name) = hello2Path
helloReadDirectory _ = return (Left (eNOENT))

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> do
                              ht <- atomicModifyIORef fuseState (\b -> (b+1,b))
                              doLog $ "Opened file " ++ (show ht)
                              return (Right ht)
                            _        -> return (Left eACCES)
    | path == hello2Path = case mode of
                            ReadOnly -> do
                              ht <- atomicModifyIORef fuseState (\b -> (b+1,b))
                              doLog $ "Opened file " ++ (show ht)
                              return (Right ht)
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead a b c d = do
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

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
