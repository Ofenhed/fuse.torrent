{-# LANGUAGE LambdaCase #-}
module Sync where

import Control.Concurrent.QSem (waitQSem, QSem, newQSem, signalQSem)
import Control.Concurrent.Chan (Chan, writeChan, readChan)
import System.IO (withFile, IOMode(AppendMode), hPrint, IOMode(WriteMode), hFlush, stderr, stdout, Handle)
import System.Mem.Weak (Weak, deRefWeak)
import Control.Exception (finally)
import GHC.IO.Handle (hDuplicateTo)
import Control.Monad (when, void)
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

mainLoop :: Chan SyncEvent -> TorrentState -> IO ThreadId
mainLoop chan torrState = do alertSem <- newQSem 0
                             forkIO $ withFile "/tmp/torrent.log" WriteMode $ \log -> do
                               hPrint log "Torrent thread started"
                               hDuplicateTo log stdout
                               hDuplicateTo log stderr
                               withTorrentSession "/tmp/torrent.session" alertSem $ mainLoop' alertSem

  where
    mainLoop' alertSem session = do
                                    traceM "In main loop starter"
                                    thread <- alertFetcher session alertSem chan
                                    traceM "Alert worker spawned"
                                    finally mainLoop'' $ killThread thread
      where
        mainLoop'' = readChan chan >>= \case
          AddTorrent magnet path -> addTorrent session magnet path >> mainLoop''
          RequestFileContent{ _callback = callback} -> signalQSem callback >> mainLoop''
          NewAlert alert -> do
            traceM "Got alert"
            traceM $ alertWhat alert
            when (alertType alert == 45 && alertWhat alert == "metadata_received")
               $ case traceShowId $ alertTorrent alert of
                   Nothing -> return ()
                   Just torrent -> deRefWeak (torrState^.fuseFiles) >>= \case
                                     Nothing -> return ()
                                     Just fs -> unpackTorrentFiles session torrent >>= \(name, filesystem) -> writeIORef fs filesystem

            -- let getTorrentInfo torrent = do
            --       name <- getTorrentName sess torrent
            --       files <- getTorrentFiles sess torrent
            --       return $ Just (name, maybe [] (\(TorrentInfo files) -> files) files)
            -- outputNew <- maybe (return Nothing) getTorrentInfo torrent
            -- hPrint log outputNew
            mainLoop''
            -- deweaked <- deRefWeak $ torrentFiles torrState
            -- case deweaked of
            -- -- case Just (fuseFiles fuseState) of
            --   Just fs -> do
            --     -- maybe (return ()) (\(name, files) -> maybe (return ()) (\name -> writeIORef fs $ traceShowId $ buildStructureFromTorrents files) name) outputNew
            --     mainLoop'
            --   Nothing -> return ()
