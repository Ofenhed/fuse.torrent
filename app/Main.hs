{-# LANGUAGE TupleSections #-}
module Main where

import Torrent
import TorrentFileSystem
import SyncTypes
import qualified Sync

import Control.Concurrent
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Exception
import Control.Lens
import Control.Monad (void, when, forM, forM_, join)
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Debug.Trace

type FuseFDType = TFSHandle

quotCeil x y = quot x y + if rem x y /= 0 then 1 else 0

defaultStates :: Chan SyncEvent -> FilePath -> FilePath -> IO (FuseState, TorrentState)
defaultStates chan rootDir stateDir = do
  ref <- newIORef emptyFileSystem
  newFiles <- newIORef Set.empty
  weak <- mkWeakIORef ref $ return ()
  return (FuseState { _files = ref, _syncChannel = chan, _hiddenDirs = [stateDir], _newFiles = newFiles }, TorrentState { _fuseFiles = weak, _statePath = joinPath [rootDir, stateDir] })

data FuseDied = FuseDied deriving Show
instance Exception FuseDied

main :: IO ()
main = do
  env <- getEnvironment
  comChannel <- newChan
  (fuseState, torrState) <- defaultStates comChannel "/tmp/" ".torrent_state"
  let torrentMain = do
        case find ((==)"TEST_TORRENT" . fst) env of
          Just (_, torr) -> writeChan comChannel $ AddTorrent Nothing $ NewMagnetTorrent torr
          Nothing -> return ()
        -- torrent <- maybe (return Nothing) (\(_, t) -> Just <$> addTorrent sess t "/tmp/torrent_test")
        -- hPrint log torrent
        Sync.mainLoop comChannel torrState
  fuseMain (myFuseFSOps fuseState torrentMain) defaultExceptionHandler

doLog str = return () -- withFile "/home/marcus/Projects/fuse.torrent/debug.log" AppendMode $ flip hPutStrLn str

myFuseFSOps :: FuseState -> IO ThreadId -> FuseOperations FuseFDType
myFuseFSOps state main = defaultFuseOps { fuseGetFileStat     = myFuseGetFileStat state
                                        , fuseOpen            = myFuseOpen state
                                        , fuseCreateDevice    = myFuseCreateDevice state
                                        , fuseCreateDirectory = myFuseCreateDirectory state
                                        , fuseRead            = myFuseRead state
                                        , fuseWrite           = myFuseWrite state
                                        , fuseRemoveDirectory = myFuseRemoveDirectory state
                                        , fuseRemoveLink      = myFuseRemoveLink state
                                        , fuseRelease         = myFuseRelease state
                                        , fuseOpenDirectory   = myFuseOpenDirectory state
                                        , fuseReadDirectory   = myFuseReadDirectory state
                                        , fuseInit            = void main
                                        , fuseDestroy         = myFuseDestroy state
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
                                           , statFileSize = filesize
                                           , statBlocks = fromIntegral $ quotCeil filesize (maybe 1 fromIntegral blocksize)
                                           , statAccessTime = 0
                                           , statModificationTime = 0
                                           , statStatusChangeTime = 0
                                           }

applyMode mode stat = stat { statFileMode = unionFileModes mode $ statFileMode stat }
applyOwnerWritable = applyMode ownerWriteMode

myFuseGetFileStat :: FuseState -> FilePath -> IO (Either Errno FileStat)
myFuseGetFileStat state "/" = Right . dirStat <$> getFuseContext
myFuseGetFileStat state ('/':path) = do
  ctx <- getFuseContext
  files <- readIORef $ state^.files
  newFiles' <- readIORef $ state^.newFiles
  let matching = getTFS' files path
  return $ case matching of
             Just (f, _) -> if isJust $ f^?contents
                                then Right $ dirStat ctx
                                else Right $ fileStat (f^?!filesize) (Just $ f^?!pieceSize) ctx
             _ -> if Set.member path newFiles'
                     then Right $ fileStat 0 Nothing ctx
                     else Left eNOENT

myFuseGetFileStat state _ =
    return $ Left eNOENT

myFuseOpenDirectory :: FuseState -> FilePath -> IO Errno
myFuseOpenDirectory _ "/" = return eOK
myFuseOpenDirectory state ('/':path) = do
  files <- readIORef $ state^.files
  let matching = getTFS' files path
  return $ case matching of
             Just (f, _) -> if isJust $ f^?contents
                               then eOK
                               else eNOENT
             Nothing -> eNOENT

myFuseReadDirectory :: FuseState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
myFuseReadDirectory state "/" = do
    ctx <- getFuseContext
    files <- readIORef $ state^.files
    traceShowM files
    newFiles <- readIORef $ state^.newFiles
    let builtin = [(".", applyOwnerWritable $ dirStat ctx)
                  ,("..", dirStat ctx)]
        directories = flip map (Map.toList files) $ \(name, file) ->
          (name, case file^?filesize of
                   Just size -> fileStat size (file^?pieceSize) ctx
                   Nothing -> dirStat ctx)
        newFiles' = map (, applyOwnerWritable $ fileStat 0 Nothing ctx) $ Set.toList newFiles
    return $ Right $ traceShowId $ builtin ++ directories --  ++ newFiles'

myFuseReadDirectory state ('/':path) = do
  ctx <- getFuseContext
  files' <- readIORef $ state^.files

  let matching = getTFS files' path
      builtin = [(".",          dirStat  ctx)
                ,("..",         dirStat  ctx)]
  return $ Right $ traceShowId $ builtin ++ map (\(name, t) -> (name, (if isJust (t^?contents) then dirStat else fileStat (t^?!filesize) (t^?pieceSize)) ctx)) matching

myFuseOpen :: FuseState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FuseFDType)
myFuseOpen fuseState ('/':path) ReadWrite _ = do
  files <- readIORef $ fuseState^.files
  case getTFS files path of
    [] -> join $ atomicModifyIORef (fuseState^.newFiles) $ \files ->
      if Set.member path files
        then (Set.delete path files, Right . NewTorrentFileHandle path <$> newIORef B.empty)
        else (files, return $ Left eACCES)
    _ -> return $ Left eACCES

myFuseOpen fuseState path WriteOnly flags = myFuseOpen fuseState path ReadWrite flags

myFuseOpen _ _ ReadWrite _ = return $ Left eACCES
myFuseOpen fuseState ('/':path) ReadOnly flags = do
    files <- readIORef $ fuseState^.files
    let matching = getTFS' files path
    case matching of
      Just (fsEntry@TFSTorrentFile{}, _) -> do
        uid <- newEmptyMVar
        writeChan (fuseState^.syncChannel) $ RequestStartTorrent { SyncTypes._torrent = fsEntry^?!TorrentFileSystem.torrent, _fdCallback = uid }
        uid' <- takeMVar uid
        buffer <- newIORef Nothing
        return $ Right $ TorrentFileHandle { _fileNoBlock = nonBlock flags
                                           , _tfsEntry = traceShowId fsEntry
                                           , _blockCache = buffer
                                           , _uid = uid' }
      _ -> return $ Left eNOENT

myFuseCreateDevice :: FuseState -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
myFuseCreateDevice state ('/':path) RegularFile _ _ =
  if takeExtension path == ".torrent"
     then atomicModifyIORef (state^.newFiles) $ \x -> (Set.insert path x, eOK)
     else return eACCES
myFuseCreateDevice _ _ _ _ _ = return eACCES

myFuseCreateDirectory :: FuseState -> FilePath -> FileMode -> IO Errno
myFuseCreateDirectory fuseState ('/':path) _ = do
  atomicModifyIORef (fuseState^.files) $ \files -> let newDir = pathToTFSDir' path
                                                 in (mergeDirectories2 files newDir, eOK)
myFuseCreateDirectory _ _ _ = return eACCES


myFuseRead :: FuseState -> FilePath -> FuseFDType -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
myFuseRead fuseState _ handle@SimpleFileHandle{} count offset = do
  pos <- hTell $ handle^?!fileHandle
  when (pos /= fromIntegral offset) $ hSeek (handle^?!fileHandle) AbsoluteSeek $ fromIntegral offset
  Right <$> B.hGet (handle^?!fileHandle) (fromIntegral count)

myFuseRead _ _ (NewTorrentFileHandle _ buffer) count offset =  Right . B.take (fromIntegral count) . B.drop (fromIntegral offset) <$> readIORef buffer

myFuseRead fuseState _ handle@TorrentFileHandle{} count offset = do
  let tfs = handle^?!tfsEntry
      pieceStart' = tfs^?!pieceStart
      pieceStartOffset' = fromIntegral $ tfs^?!pieceStartOffset
      pieceSize' = fromIntegral $ tfs^?!pieceSize
      totalFirstPieceOffset = pieceStartOffset' + offset
      firstPiece' = pieceStart' + fromIntegral (quot totalFirstPieceOffset (fromIntegral pieceSize'))

      actualFirstPieceOffset = mod totalFirstPieceOffset $ fromIntegral pieceSize'
      spaceInFirstPiece = pieceSize' - actualFirstPieceOffset
      afterFirstPiece = max 0 $ fromIntegral count - spaceInFirstPiece
      additionalPieces = quotCeil afterFirstPiece pieceSize'
      pieces = 1 + additionalPieces
      fittingCount = (tfs^?!filesize) - offset
  maybeFirstBlock <- readIORef $ handle^?!blockCache
  let maybeFirstBlock' = case maybeFirstBlock of
                           Just (cachePiece, cacheBuf) ->
                             if cachePiece == firstPiece'
                               then Just cacheBuf
                               else Nothing
                           _ -> Nothing
      firstBlockModifier = case maybeFirstBlock' of
                             Just _ -> 1
                             Nothing -> 0
      numPieces = fromIntegral pieces - firstBlockModifier
      firstFetchedPiece = fromIntegral firstPiece' + firstBlockModifier
  retChans <- mapM (\piece -> newEmptyMVar >>= \chan -> return (chan, piece)) $ take (fromIntegral numPieces) [firstFetchedPiece..]
  traceShowM $ map (^._2) retChans
  forM_ retChans $ \(chan, piece) ->
    let req = RequestFileContent { SyncTypes._torrent = tfs^?!TorrentFileSystem.torrent
                               , _piece = fromIntegral piece
                               , _count = 1
                               , _fileData = chan }
     in writeChan (fuseState^.syncChannel) req
  if handle^?!fileNoBlock
     then return $ Left eWOULDBLOCK
     else do
       returnedData <- forM retChans $ \(chan, piece) -> (piece,) <$> takeMVar chan
       when (numPieces > 0) $ writeIORef (handle^?!blockCache) $ Just $ last returnedData
       let unnumberedData = map (^._2) returnedData
       let wantedData = B.take (fromIntegral fittingCount) $ B.drop (fromIntegral actualFirstPieceOffset) $ B.concat $ maybe unnumberedData (:unnumberedData) maybeFirstBlock'
       return $ Right wantedData

myFuseWrite :: FuseState -> FilePath -> FuseFDType -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
myFuseWrite state _ fh@(NewTorrentFileHandle _ buffer) input offset = atomicModifyIORef buffer $ \buffer' ->
  if offset == fromIntegral (B.length buffer')
     then (B.append buffer' input, Right $ fromIntegral $ B.length input)
     else (buffer', Left eWOULDBLOCK)

myFuseRemoveDirectory :: FuseState -> FilePath -> IO Errno
myFuseRemoveDirectory fuseState ('/':path) = do
  files <- readIORef $ fuseState^.files
  case traceShowId $ getTFS' files path of
    Just (TFSTorrentDir { TorrentFileSystem._torrent = torrent }, _) ->
      writeChan (fuseState^.syncChannel) (RemoveTorrent torrent) >> return eOK
    Just (TFSDir { _contents = contents}, _) -> return eACCES
    _ -> return eNOENT

myFuseRemoveLink :: FuseState -> FilePath -> IO Errno
myFuseRemoveLink fuseState ('/':path) = do
  files <- readIORef $ fuseState^.files
  case traceShowId $ getTFS' files path of
    Just (TFSTorrentFile { TorrentFileSystem._torrent = torrent, _singleFileTorrent = True }, _) ->
      writeChan (fuseState^.syncChannel) (RemoveTorrent torrent) >> return eOK
    _ -> return eNOENT

myFuseRelease :: FuseState -> FilePath -> FuseFDType -> IO ()
myFuseRelease _ _ fh@SimpleFileHandle{} = hClose $ fh^?!fileHandle

myFuseRelease fuseState _ fh@TorrentFileHandle{} = writeChan (fuseState^.syncChannel) $ CloseTorrent { SyncTypes._torrent = fh^?!tfsEntry^?!TorrentFileSystem.torrent, _fd = fh^?!uid }

myFuseRelease fuseState _ fh@(NewTorrentFileHandle path content) =
  readIORef content >>= writeChan (fuseState^.syncChannel) . AddTorrent (Just $ takeDirectory path) . NewTorrentFile

myFuseDestroy :: FuseState -> IO ()
myFuseDestroy fuseState = do
  torrentDead <- newEmptyMVar
  writeChan (fuseState^.syncChannel) $ FuseDead torrentDead
  void $ takeMVar torrentDead
  traceM "Torrent is dead, finalizing"
