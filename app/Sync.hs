{-# LANGUAGE LambdaCase #-}
module Sync where

import Control.Concurrent.Chan (Chan, writeChan, readChan)
import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.QSem (waitQSem, QSem, newQSem, signalQSem)
import Control.Exception (finally)
import Control.Lens ((^.), (^?!), over, set)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Monad (when, void, unless)
import Data.IORef
import Data.Map.Strict
import Data.Maybe (fromJust, fromMaybe)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
import System.Mem.Weak (Weak, deRefWeak)

import qualified Data.ByteString as B

import SyncTypes
import Torrent
import TorrentFileSystem
import TorrentTypes

import Debug.Trace

alertFetcher :: TorrentSession -> QSem -> Chan SyncEvent -> IO ThreadId
alertFetcher sess alertSem chan = forkIO alertLoop
  where alertLoop = popAlert sess >>= \case
                      Nothing -> waitQSem alertSem >> alertLoop
                      Just alert -> writeChan chan (NewAlert alert) >> alertLoop

unpackTorrentFiles sess torrent = do
  name <- getTorrentName sess torrent
  files <- getTorrentFiles sess torrent
  let filesystem = maybe [] (buildStructureFromTorrentInfo torrent) files
  return (name, filesystem)

mainLoop :: Chan SyncEvent -> TorrentState -> IO ThreadId
mainLoop chan torrState = do alertSem <- newQSem 0
                             forkIO $ withFile "/tmp/torrent.log" WriteMode $ \log -> do
                               hPrint log "Torrent thread started"
                               -- hDuplicateTo log stdout
                               -- hDuplicateTo log stderr
                               withTorrentSession "/tmp/torrent.session" alertSem $ mainLoop' alertSem

  where
    mainLoop' alertSem session = do
                                    thread <- alertFetcher session alertSem chan
                                    let looper = mainLoop'' >> get >>= \case 
                                                                     KillSyncThread -> return ()
                                                                     _ -> looper
                                    finally (evalStateT looper $ SyncThreadState empty) $ killThread thread
      where
        mainLoop'' :: StateT SyncState IO ()
        mainLoop'' = do
          d <- liftIO $ readChan chan
          case d of
            AddTorrent magnet path -> void $ liftIO $ addTorrent session magnet path
            RequestStartTorrent { SyncTypes._torrent = torrent } -> void $ liftIO $ startTorrent session torrent
            RequestFileContent{ SyncTypes._torrent = torrent
                              , _piece = piece
                              , _callback = callback } -> do
                                let key = (torrent, piece)
                                state <- get
                                unless (member key $ state^.inWait) $
                                  void $ liftIO $ downloadTorrentParts session torrent piece 100 25
                                let newState = over inWait (flip alter key $ maybe (Just [callback]) (Just . (callback:))) state
                                put newState
            FuseDead -> put KillSyncThread
            NewAlert alert ->
              case (alert^.alertType, alert^.alertWhat) of
                (45, "metadata_received") ->
                  case alert^.alertTorrent of
                    Nothing -> return ()
                    Just torrent -> do
                      ref <- liftIO $ deRefWeak (torrState^.fuseFiles)
                      case ref of
                        Nothing -> put KillSyncThread
                        Just fs -> liftIO $ unpackTorrentFiles session torrent >>= \(name, filesystem) -> void $ liftIO $ writeIORef fs filesystem
                (5, "read_piece") -> do
                  let key = (fromJust $ alert^.alertTorrent, alert^.alertPiece)
                      d = fromMaybe B.empty $ alert^.alertBuffer
                  state <- get
                  case updateLookupWithKey (const $ const Nothing) key $ state^.inWait of
                    (Just inWaitForPiece, newMap) -> do
                      liftIO $ mapM_ (`writeChan` d) inWaitForPiece
                      put $ set inWait newMap state
                    _ -> return ()

                _ -> return ()
