{-# LANGUAGE LambdaCase #-}
module Sync where

import Control.Concurrent.Chan (Chan, writeChan, readChan)
import Control.Concurrent (forkIO, ThreadId, killThread, tryPutMVar, newEmptyMVar, tryTakeMVar)
import Control.Concurrent.QSem (waitQSem, QSem, newQSem, signalQSem)
import Control.Exception (finally)
import Control.Lens ((^.), (^?!), over, set)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Monad (when, void, unless, forM_)
import Data.IORef
import Data.Map.Strict
import Data.Maybe (fromJust, fromMaybe)
import GHC.IO.Handle (hDuplicateTo)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (joinPath, takeExtension)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)

import qualified Data.ByteString as B

import SyncTypes
import Torrent
import TorrentFileSystem
import TorrentTypes

import Debug.Trace

alertFetcher :: Weak TorrentSession -> QSem -> Chan SyncEvent -> IO ThreadId
alertFetcher sess alertSem chan = forkIO alertLoop
  where alertLoop = deRefWeak sess >>= \case
                      Just sess -> popAlert sess >>= \case
                        Nothing -> waitQSem alertSem >> alertLoop
                        Just alert -> writeChan chan (NewAlert alert) >> alertLoop
                      Nothing -> return ()

unpackTorrentFiles sess torrent = do
  name <- getTorrentName sess torrent
  files <- getTorrentFiles sess torrent
  let filesystem = maybe [] (buildStructureFromTorrentInfo torrent) files
  return (name, filesystem)

torrentDir x = joinPath [x^.statePath, "torrents"]
torrentDataDir x = joinPath [x^.statePath, "data"]

mainLoop :: Chan SyncEvent -> TorrentState -> IO ThreadId
mainLoop chan torrState = do
  alertSem <- newQSem 0
  forkIO $ withFile "/tmp/torrent.log" WriteMode $ \log -> do
    hPrint log "Torrent thread started"
    createDirectoryIfMissing True $ torrState^.statePath
    createDirectoryIfMissing True $ torrentDir torrState
    createDirectoryIfMissing True $ torrentDataDir torrState
    -- hDuplicateTo log stdout
    -- hDuplicateTo log stderr
    withTorrentSession (joinPath [torrState^.statePath, ".session"]) alertSem $ mainLoop' alertSem

  where
    mainLoop' alertSem session = do
        files <- listDirectory $ torrentDir torrState
        forM_ files $ \filename -> do
          resumeData <- B.readFile $ joinPath [torrentDir torrState, filename]
          resumeTorrent session resumeData $ torrentDataDir torrState
        weakSession <- mkWeakPtr session Nothing
        alertThread <- alertFetcher weakSession alertSem chan
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
              void $ maybe (return False) (`tryPutMVar` True) finishCallback'
            collectResumeDatas 0 = trace "Done, killing alert thread" $ killThread alertThread
            collectResumeDatas count = traceShow ("Working for", count) $ liftIO (readChan chan) >>= \case
              NewAlert alert -> case traceShowId alert of
                                  Alert { _alertWhat = "save_resume_data"
                                        , _alertTorrent = Just torr
                                        , _alertBuffer = Just buff } -> do
                                    traceShowM ("Saving torrent", torr, B.length buff)
                                    B.writeFile (joinPath [torrentDir torrState, handleToHex torr ++ ".torrent"]) buff
                                    collectResumeDatas $ count - 1
                                  _ -> trace "Wrong alert, retrying" $ collectResumeDatas count
              _ -> trace "No alert, retrying" $ collectResumeDatas count

        finally (evalStateT looper $ SyncThreadState empty) finisher
      where
        mainLoop'' :: StateT SyncState IO ()
        mainLoop'' = liftIO (readChan chan) >>= \case
            AddTorrent magnet -> void $ liftIO $ addTorrent session magnet $ torrentDataDir torrState
            RequestStartTorrent { SyncTypes._torrent = torrent, _fd = callback } -> void $ liftIO $ startTorrent session torrent >> tryPutMVar callback 0
            RequestFileContent { SyncTypes._torrent = torrent
                               , _piece = piece
                               , _fileData = callback } -> do
                                 let key = (torrent, piece)
                                 state <- get
                                 unless (member key $ state^.inWait) $
                                   void $ liftIO $ downloadTorrentParts session torrent piece 100 25
                                 let newState = over inWait (flip alter key $ maybe (Just [callback]) (Just . (callback:))) state
                                 put newState
            FuseDead mvar -> put $ KillSyncThread mvar
            NewAlert alert -> traceShow (alert^.alertWhat, alert^.alertType, alert^.alertPiece) $
              case (alert^.alertType, alert^.alertWhat) of
                (67, "add_torrent") ->
                  case alert^.alertTorrent of
                    Nothing -> return ()
                    Just torrent -> do
                      ref <- liftIO $ deRefWeak (torrState^.fuseFiles)
                      case ref of
                        Nothing -> error "This should not happen unless FuseDead has already been received"
                        Just fs -> liftIO $ unpackTorrentFiles session torrent >>= \(name, filesystem) -> void $ liftIO $ atomicModifyIORef fs $ \before -> (mergeDirectories $ before ++ filesystem, ())
                (45, "metadata_received") ->
                  case alert^.alertTorrent of
                    Nothing -> return ()
                    Just torrent -> do
                      ref <- liftIO $ deRefWeak (torrState^.fuseFiles)
                      case ref of
                        Nothing -> error "This should not happen unless FuseDead has already been received"
                        Just fs -> liftIO $ unpackTorrentFiles session torrent >>= \(name, filesystem) -> void $ liftIO $ atomicModifyIORef fs $ \before -> (mergeDirectories $ before ++ filesystem, ())
                (5, "read_piece") -> do
                  let key = (fromJust $ alert^.alertTorrent, alert^.alertPiece)
                      d = fromMaybe B.empty $ alert^.alertBuffer
                  state <- get
                  case updateLookupWithKey (const $ const Nothing) key $ state^.inWait of
                    (Just inWaitForPiece, newMap) -> do
                      liftIO $ mapM_ (`tryPutMVar` d) inWaitForPiece
                      put $ set inWait newMap state
                    _ -> return ()

                _ -> return ()
