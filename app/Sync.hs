{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Sync where

import Prelude hiding (lookup)

import Control.Concurrent.Chan (Chan, writeChan, readChan)
import Control.Concurrent (forkIO, forkOS, ThreadId, killThread, tryPutMVar, newEmptyMVar, tryTakeMVar)
import Control.Exception (finally)
import Control.Lens ((^.), (^?), (^?!), over, set)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Monad (when, void, unless, forM_)
import Data.Bits (toIntegralSized)
import Data.IORef
import Data.List ((\\), foldl', union)
import Data.Map.Strict (Map, delete, updateLookupWithKey, alter, member, empty, keys, insert, lookup)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import GHC.IO.Handle (hDuplicateTo)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (joinPath, takeExtension)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import SyncTypes
import Torrent
import TorrentFileSystem
import TorrentTypes

import Debug.Trace

alertFetcher :: Weak TorrentSession -> IO () -> Chan SyncEvent -> IO ThreadId
alertFetcher sess alertWait chan = forkOS alertLoop
  where alertLoop = deRefWeak sess >>= \case
                      Just sess -> popAlerts sess >>= \case
                        [] -> alertWait >> alertLoop
                        alerts -> mapM_ (writeChan chan . NewAlert) alerts >> alertLoop
                      Nothing -> return ()

unpackTorrentFiles sess torrent = do
  name <- getTorrentName sess torrent
  files <- getTorrentFiles sess torrent
  let filesystem = maybe emptyFileSystem (buildStructureFromTorrentInfo torrent) files
  return (name, filesystem)

torrentDir x = joinPath [x^.statePath, "torrents"]
torrentDataDir x = joinPath [x^.statePath, "data"]

insertFirstMap :: (Bounded a, Enum a, Ord a) => Map a b -> b -> (Map a b, a)
insertFirstMap into value = let position = head $ [minBound..] \\ keys into
                           in (insert position value into, position)

mainLoop :: Chan SyncEvent -> TorrentState -> IO ThreadId
mainLoop chan torrState = do
  forkIO $ withFile "/tmp/torrent.log" WriteMode $ \log -> do
    hPrint log "Torrent thread started"
    createDirectoryIfMissing True $ torrState^.statePath
    createDirectoryIfMissing True $ torrentDir torrState
    createDirectoryIfMissing True $ torrentDataDir torrState
    -- hDuplicateTo log stdout
    -- hDuplicateTo log stderr
    withTorrentSession (joinPath [torrState^.statePath, ".session"]) mainLoop'

  where
    mainLoop' alertWait session = do
        files <- listDirectory $ torrentDir torrState
        forM_ files $ \filename -> do
          resumeData <- B.readFile $ joinPath [torrentDir torrState, filename]
          resumeTorrent session resumeData $ torrentDataDir torrState
        setTorrentSessionActive session True
        weakSession <- mkWeakPtr session Nothing
        alertThread <- alertFetcher weakSession alertWait chan
        finishCallback <- newEmptyMVar
        let looper = mainLoop'' >> get >>= \case
                                         KillSyncThread callback -> void $ liftIO $ tryPutMVar finishCallback callback
                                         _ -> looper
            finisher = do
              setTorrentSessionActive session False
              count <- requestSaveTorrentResumeData session
              traceShowM ("Collecting torrents", count)
              collectResumeDatas count
              finishCallback' <- tryTakeMVar finishCallback
              void $ maybe (return False) (`tryPutMVar` ()) finishCallback'
            collectResumeDatas 0 = trace "Done, killing alert thread" $ killThread alertThread
            collectResumeDatas count = traceShow ("Working for", count) $ liftIO (readChan chan) >>= \case
              NewAlert alert -> case traceShowId alert of
                                  Alert { _alertWhat = "save_resume_data"
                                        , _alertTorrent = Just torr
                                        , _alertBuffer = Just buff } -> do
                                    torrentHash <- liftIO $ getTorrentHash torr
                                    traceShowM ("Saving torrent", torr, B.length buff)
                                    B.writeFile (joinPath [torrentDir torrState, hashToHex torrentHash ++ ".torrent"]) buff
                                    collectResumeDatas $ count - 1
                                  Alert { _alertWhat = "save_resume_data_failed" } -> do
                                    collectResumeDatas $ count - 1
                                  _ -> trace "Wrong alert, retrying" $ collectResumeDatas count
              _ -> trace "No alert, retrying" $ collectResumeDatas count

        finally (evalStateT looper $ SyncThreadState empty empty empty) finisher
      where
        mainLoop'' :: StateT SyncState IO ()
        mainLoop'' = liftIO (readChan chan) >>= \case

            AddTorrent path torrentData -> do
              newTorrent <- liftIO $ addTorrent session torrentData $ torrentDataDir torrState
              case (newTorrent, path) of
                (Just newTorrent', Just path') -> do
                  state <- get
                  let state' = state { _newTorrentPath = alter (const $ Just path') newTorrent' $ state^?!newTorrentPath }
                  put state'
                _ -> return ()

            OpenTorrent { SyncTypes._torrent = torrent, _fdCallback = callback, _piece = piece } -> do
              state <- get
              torrentHash <- liftIO $ getTorrentHash torrent
              let fdsForTorrent = fromMaybe empty $ lookup torrentHash $ state^?!fds
                  (newFdsForTorrent, fd) = insertFirstMap fdsForTorrent piece
                  state' = state { _fds = alter (const $ Just newFdsForTorrent) torrentHash $ state ^?!fds }
              put state'
              when (null fdsForTorrent) $ void $ liftIO $ resetTorrent session torrent
              void $ liftIO $ tryPutMVar callback fd

            CloseTorrent { SyncTypes._torrent = torrent, _fd = fd } -> do
              state <- get
              torrentHash <- liftIO $ getTorrentHash torrent
              let fdsForTorrent = fromMaybe empty $ lookup torrentHash $ state^?!fds
                  newFdsForTorrent = delete fd fdsForTorrent
                  noFdsLeft = null newFdsForTorrent
                  newFdsForTorrent' = if noFdsLeft then Nothing else Just newFdsForTorrent
                  state' = state { _fds = alter (const newFdsForTorrent') torrentHash $ state^?!fds }
              put state'
              when noFdsLeft $ void $ liftIO $ resetTorrent session torrent

            RemoveTorrent { SyncTypes._torrent = target } -> do
              ref <- liftIO $ deRefWeak (torrState^.fuseFiles)
              let filterTorrent x = if x^?TorrentFileSystem.torrent == Just target
                                      then Nothing
                                      else case x^?contents of
                                             Just children -> Just $ x { _contents = Map.mapMaybe filterTorrent children }
                                             Nothing -> Just x
              case ref of
                Just fs -> void $ liftIO $ atomicModifyIORef fs $ \x -> (Map.mapMaybe filterTorrent x, ())
                Nothing -> return ()

            ReadTorrent { _fileData = [] } -> return ()
            ReadTorrent { SyncTypes._torrent = torrent
                        , _fd = fd
                        , _piece = piece
                        , _fileData = callbacks } -> do
              torrentHash <- liftIO $ getTorrentHash torrent
              let key = (torrentHash, piece)
              oldState <- get
              let newInWait = foldl' (\prevState (callback, piece_offset) -> over inWait (flip alter (torrentHash, piece + piece_offset)
                                                                                                     $ maybe (Just [callback]) (Just . (callback:))) prevState)
                                     oldState
                                     (zip callbacks [0..])
                  currFdPiece = piece + (fromJust . toIntegralSized . length) callbacks - 1
                  newState = over fds (flip alter torrentHash $ maybe (Just $ Map.singleton fd currFdPiece) $ Just . Map.insert fd currFdPiece) newInWait
              put newState
              unless (member key $ oldState^.inWait) $ do
                let requestedPieces = flip mapMaybe (keys $ newState^.inWait)
                                                    $ \(hash, piece) ->
                                                      if torrentHash == hash
                                                         then Just piece
                                                         else Nothing
                    fdPositions = maybe [] Map.elems (lookup torrentHash $ newState^.fds)
                void $ liftIO $ downloadTorrentParts session torrent (requestedPieces `union` fdPositions) 1000 25

            FuseDead mvar -> put $ KillSyncThread mvar

            NewAlert alert ->
              let publishTorrent torrent = do
                      ref <- liftIO $ deRefWeak (torrState^.fuseFiles)
                      case ref of
                        Nothing -> return ()
                        Just fs -> do
                          state <- get
                          torrentHash <- liftIO $ getTorrentHash torrent
                          let (path, state') = Map.updateLookupWithKey (const $ const Nothing) torrentHash $ state^?!newTorrentPath
                              createPath = maybe id (flip pathToTFSDir) path
                          (name, filesystem) <- liftIO $ unpackTorrentFiles session torrent
                          unless (null filesystem) $ do
                             void $ liftIO $ atomicModifyIORef fs $ \before -> (mergeDirectories2 before $ createPath filesystem, ())
                             put $ state { _newTorrentPath = state' }
               in case (alert^.alertType, alert^.alertWhat) of
                    (67, "add_torrent") -> forM_ (alert^.alertTorrent) publishTorrent
                    (45, "metadata_received") -> forM_ (alert^.alertTorrent) publishTorrent

                    (5, "read_piece") -> do
                      torrentHash <- liftIO $ getTorrentHash $ fromJust $ alert^.alertTorrent
                      let key = (torrentHash, alert^.alertPiece)
                          d = fromMaybe B.empty $ alert^.alertBuffer

                      state <- get
                      case updateLookupWithKey (const $ const Nothing) key $ state^.inWait of
                        (Just inWaitForPiece, newMap) -> do
                          liftIO $ mapM_ (`tryPutMVar` d) inWaitForPiece
                          put $ set inWait newMap state
                        _ -> return ()

                    (43, "file_error") -> case alert^.alertTorrent of
                      Just torrent -> liftIO $ checkTorrentHash session torrent
                      Nothing -> return ()

                    (41, "torrent_checked") -> case alert^.alertTorrent of
                      Nothing -> return ()
                      Just torrent -> do
                        state <- get
                        torrentHash <- liftIO $ getTorrentHash torrent
                        let pieces = flip mapMaybe (keys $ state^.inWait) $ \(hash, piece) -> if torrentHash == hash
                                                                                                 then Just piece
                                                                                                 else Nothing
                        void $ liftIO $ downloadTorrentParts session torrent pieces 1000 25

                    _ -> return ()
