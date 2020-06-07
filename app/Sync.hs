{-# LANGUAGE LambdaCase #-}
module Sync where

import Control.Concurrent.QSem (waitQSem, QSem, newQSem, signalQSem)
import Control.Concurrent.Chan (Chan, writeChan, readChan)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
import System.Mem.Weak (Weak, deRefWeak)
import Control.Exception (finally)
import GHC.IO.Handle (hDuplicateTo)
import Control.Monad (when, void)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (^?!))
import Control.Concurrent (forkIO, ThreadId, killThread)
import Data.IORef
import TorrentFileSystem

import Torrent
import TorrentTypes
import SyncTypes

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

data SyncState = SyncThreadState { _inWait :: [(TorrentHandle, TorrentPieceSizeType)] }
               | KillSyncThread
               deriving Show

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
                                    finally (evalStateT looper $ SyncThreadState []) $ killThread thread
      where
        mainLoop'' :: StateT SyncState IO ()
        mainLoop'' = do
          d <- liftIO $ readChan chan
          case d of
            AddTorrent magnet path -> void $ liftIO $ addTorrent session magnet path
            RequestStartTorrent { SyncTypes._torrent = torrent
                                , _callback = callback } -> liftIO $ startTorrent session torrent >> signalQSem callback
            RequestFileContent{ SyncTypes._torrent = torrent
                              , _piece = piece
                              , _count = count
                              , _callback = callback } -> void $ liftIO $ downloadTorrentParts session torrent piece count 100
            FuseDead -> put KillSyncThread
            NewAlert alert -> traceShow alert $ when (alert^.alertType == 45 && alert^.alertWhat == "metadata_received") $
                 case alert^.alertTorrent of
                   Nothing -> return ()
                   Just torrent -> do
                     ref <- liftIO $ deRefWeak (torrState^.fuseFiles)
                     case ref of
                       Nothing -> put KillSyncThread
                       Just fs -> liftIO $ unpackTorrentFiles session torrent >>= \(name, filesystem) -> void $ liftIO $ writeIORef fs filesystem
