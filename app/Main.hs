{-# LANGUAGE LambdaCase #-}
module Main where

import Torrent
import qualified Data.ByteString.Char8 as B
import Control.Exception
import System.Exit (exitWith, ExitCode(..))
import System.Mem.Weak (Weak, deRefWeak)
import System.Environment (getEnvironment)
import Data.List (find, partition)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import Foreign.C.Error
import System.Posix.Types
import GHC.IO.Handle (hDuplicateTo)
import Control.Monad (void)
import System.Posix.Files
import Control.Concurrent
import Data.IORef
import System.FilePath
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.ExecutionStack (showStackTrace)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
-- import System.Posix.IO

import System.Fuse

import Debug.Trace

data TorrentFileSystemEntry = TFSTorrentFile TorrentHandle String
                            | TFSTorrentDir TorrentHandle String TorrentFileSystemEntryList
                            | TFSFile String Word
                            | TFSDir String TorrentFileSystemEntryList
                            deriving Show
type TorrentFileSystemEntryList = [TorrentFileSystemEntry]

newtype FuseState = FuseState { fuseFiles :: IORef [TorrentFileSystemEntry] }
newtype TorrentState = TorrentState { torrentFiles :: Weak (IORef [TorrentFileSystemEntry]) }

defaultStates :: IO (FuseState, TorrentState)
defaultStates = do
  ref <- newIORef []
  weak <- mkWeakIORef ref $ return ()
  return (FuseState { fuseFiles = ref }, TorrentState { torrentFiles = weak })

type HT = ()

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

tfsMetaData (TFSTorrentFile th name) = (Just th, name, Nothing)
tfsMetaData (TFSTorrentDir th name content) = (Just th, name, Just content)
tfsMetaData (TFSFile name _) = (Nothing, name, Nothing)
tfsMetaData (TFSDir name content) = (Nothing, name, Just content)

tfsContent a = let (_, _, content) = tfsMetaData a in content
tfsTorrent a = let (th, _, _) = tfsMetaData a in th
tfsName a = let (_, name, _) = tfsMetaData a in name

tfsFilesize (TFSFile _ size) = size
tfsFilesize _ = 0

mergeDirectories :: TorrentFileSystemEntryList -> TorrentFileSystemEntryList
mergeDirectories [] = []
mergeDirectories [x] = [x]
mergeDirectories (curr:xs) = let (th, name, content) = tfsMetaData curr
                                 (same, notsame) = partition (\case
                                                                 TFSTorrentDir th' name' content' -> Just th' == th && name' == name
                                                                 TFSDir name' content' -> name' == name && isNothing th
                                                                 _ -> False) xs
                                 merge (TFSTorrentDir th name content1) (TFSTorrentDir _ _ content2) = TFSTorrentDir th name (mergeDirectories $ content1 ++ content2)
                                 merge (TFSDir name content1) (TFSDir _ content2) = TFSDir name (mergeDirectories $ content1 ++ content2)
                                 same' = foldr merge curr same
                               in same':mergeDirectories notsame



buildStructureFromTorrents :: [TorrentFile] -> TorrentFileSystemEntryList
buildStructureFromTorrents filelist = let toTfsFile torr@(Torrent.TorrentFile _ size) = TFSFile (snd $ splitname torr) size
                                          splitname (Torrent.TorrentFile name _) = splitFileName name
                                          structure = flip map filelist $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\child dir -> TFSDir dir [child]) (toTfsFile torrfile) $ splitDirectories dirs
                                        in mergeDirectories structure

getTFS :: TorrentFileSystemEntryList -> String -> TorrentFileSystemEntryList
getTFS files dirs = getTFS' files $ splitDirectories dirs
  where 
    getTFS' [] _ = []
    getTFS' files [] = files
    getTFS' files (dir:xs) = unpackGetTFS' (filter (\a -> let (_, name, _) = tfsMetaData a in name == dir) files) xs
    unpackGetTFS' files [] = files
    unpackGetTFS' files dir = getTFS' (concat $ mapMaybe tfsContent files) dir


main :: IO ()
main = withFile "/tmp/torrent.log" WriteMode $ \log -> do
  env <- getEnvironment
  (fuseState, torrState) <- defaultStates
  let torrentMain = withTorrentSession "/tmp/torrent_session.sess" $ \sess -> do
        hDuplicateTo log stdout
        hDuplicateTo log stderr
        torrent <- maybe (return Nothing) (\(_, t) -> Just <$> addTorrent sess t "/tmp/torrent_test") (find ((==)"TEST_TORRENT" . fst) env)
        hPrint log sess
        hPrint log torrent
        let mainLoop = do
              a <- waitForAlert sess 1000
              hPrint log a
              hFlush log
              let getTorrentInfo torrent = do
                    name <- getTorrentName sess torrent
                    files <- getTorrentFiles sess torrent
                    return $ Just (name, maybe [] (\(TorrentInfo files) -> files) files)
              outputNew <- maybe (return Nothing) getTorrentInfo torrent
              hPrint log outputNew
              deweaked <- deRefWeak $ torrentFiles torrState
              -- case deweaked of
              case Just (fuseFiles fuseState) of 
                Just fs -> do
                  maybe (return ()) (\(name, files) -> maybe (return ()) (\name -> writeIORef fs $ traceShowId $ buildStructureFromTorrents files) name) outputNew
                  mainLoop
                Nothing -> return ()
        hPrint log "Before mainLoop"
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
                            , fuseInit = void $ forkIO main
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
