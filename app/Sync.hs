{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Sync where

import Alert (Alerts, newAlertContainer, popAlerts, alertSaveResumeDataBuffer, alertTorrent, alertWhat, alertCategory, alertType, alertReadPiece, withAlertPtr, nextAlert, withAlertPtr_, withAlertPtr_', withAlertPtr', alertTorrentDeletedHash)
import Control.Concurrent (ThreadId, forkIO, forkOS, killThread, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVar, writeTVar, TChan, tryReadTChan)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (bracket, finally)
import Control.Lens (over, set, (^.), (^?), (^?!))
import Control.Monad (forM_, unless, void, when, forM, join)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.STM as STM
import Control.Monad.State (StateT (runStateT), evalStateT, get, put, MonadTrans (lift), modify)
import Data.Bits (toIntegralSized)
import qualified Data.ByteString as B
import Data.List (union)
import Data.Map.Strict (Map, alter, delete, empty, insert, keys, lookup, member, updateLookupWithKey, alterF, partitionWithKey)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import SyncTypes
  ( SyncEvent (..),
    SyncState (..),
    TorrentState(_torrentTrace),
    fdCursors,
    fuseFiles,
    fuseLostFound,
    inWait,
    newTorrentPath,
    statePath, writeTorrentReadCallback,
  )
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (joinPath, (</>))
import System.IO (Handle, hClose, hPrint, withFile, IOMode (WriteMode))
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import System.Posix (Fd, OpenFileFlags (directory), OpenMode (WriteOnly), changeWorkingDirectoryFd, fdToHandle, openFdAt)
import Torrent
  ( TorrentAlert (Alert, _alertBuffer, _alertTorrent, _alertWhat),
    TorrentMode(TorrentUploadOnly, TorrentDownload),
    addTorrent,
    checkTorrentHash,
    downloadTorrentParts,
    getTorrentFiles,
    getTorrentHash,
    getTorrentName,
    removeTorrent,
    requestSaveTorrentResumeData,
    resetTorrent,
    resumeTorrent,
    setTorrentSessionActive,
    withTorrentSession,
  )
import TorrentFileSystem
  ( New (New),
    TorrentFileSystemEntry (_contents),
    buildStructureFromTorrentInfo,
    contents,
    emptyFileSystem,
    hash,
    mergeDirectories2,
    mergeDuplicatesFrom,
    pathToTFSDir,
  )
import TorrentTypes
  ( TorrentSession,
    hashToHex,
  )
import Prelude hiding (lookup)
import System.IO.Error (catchIOError)
import Utils (OptionalDebug(..))

doOnce :: IO a -> IO (IO a)
doOnce a = do
  -- States of s:
  -- \* Nothing: State has not been requested
  -- \* Just Nothing: Someone has taken ownership of calculating the result, but it has not been completed
  -- \* Just Just x: Value is calculated
  s <- newTVarIO Nothing
  return $ do
    now' <- atomically $ do
      now <- readTVar s
      case now of
        Nothing -> do
          writeTVar s $ Just Nothing
          return Nothing
        Just Nothing -> STM.retry
        Just v@(Just _) -> return v
    case now' of
      Just x -> return x
      Nothing -> do
        x <- a
        atomically $ writeTVar s $ Just $ Just x
        return x

data AlertListState
  = AlertsWaiting Alerts
  | AlertsActive Alerts
  | AlertsShutdown

data AlertFetcherState = AlertFetcherState
  { alertTorrentSession :: Weak TorrentSession,
    alertState :: TVar AlertListState,
    alertWait :: IO ()
  }

alertFetcher :: AlertFetcherState -> IO ThreadId
alertFetcher AlertFetcherState {alertTorrentSession = sess, alertWait = alertWait', alertState = alertState'} = forkOS alertLoop
  where
    alertLoop =
      waitWhenBusy >>= \case
        AlertsShutdown -> return ()
        AlertsActive _ -> undefined
        AlertsWaiting alerts -> do
          deRefWeak sess >>= \case
            Just sess' ->
              popAlerts sess' alerts >>= \case
                0 -> alertWait' >> alertLoop
                x -> atomically (writeTVar alertState' $ AlertsActive alerts) >> alertLoop
            Nothing -> return ()
    waitWhenBusy = atomically $ do
      s <- readTVar alertState'
      case s of
        AlertsActive _ -> STM.retry
        x -> return x

unpackTorrentFiles debug sess torrent hash = do
  name <- getTorrentName sess torrent
  files <- getTorrentFiles sess torrent
  traceShowM debug ("Torrent files", files)
  let filesystem = maybe emptyFileSystem (buildStructureFromTorrentInfo (torrent, hash)) files
  return (name, filesystem)

torrentDir x = joinPath [x ^. statePath, "torrents"]

torrentDataDir x = joinPath [x ^. statePath, "data"]

insertFirstMap :: (Bounded a, Enum a, Ord a) => Map a b -> b -> (Map a b, a)
insertFirstMap into value = (insert minBound value into, minBound)

listDirectoryAt :: Fd -> FilePath -> IO [FilePath]
listDirectoryAt fd p = changeWorkingDirectoryFd fd >> listDirectory p

withFileAt :: Fd -> FilePath -> OpenMode -> OpenFileFlags -> (Handle -> IO a) -> IO a
withFileAt fd p m o f =
  bracket
    (openFdAt (Just fd) p m o >>= fdToHandle)
    (hClose)
    f

mainLoop :: TChan SyncEvent -> TorrentState -> IO ThreadId
mainLoop chan torrState = do
  alertQueue <- newTVarIO []
  forkIO $ do
    traceM torrState "Torrent thread started"
    createDirectoryIfMissing True $ torrState ^. statePath
    createDirectoryIfMissing True $ torrentDir'
    createDirectoryIfMissing True $ torrentDataDir'
    -- hDuplicateTo log stdout
    -- hDuplicateTo log stderr
    withTorrentSession (traceShowId torrState $ joinPath [torrState ^. statePath, ".session"]) mainLoop'
  where
    torrentDir' = torrentDir torrState
    torrentDataDir' = torrentDataDir torrState
    torrentResumeFile torrentHash = torrentDir' </> hashToHex torrentHash ++ ".torrent"
    mainLoop' alertWait' session = do
      files <- listDirectory $ torrentDir torrState
      forM_ files $ \filename -> do
        resumeData <- B.readFile $ traceShowId torrState $ joinPath [torrentDir', filename]
        resumeTorrent session resumeData
      setTorrentSessionActive session True
      alerts <- newAlertContainer
      weakSession <- mkWeakPtr session Nothing
      alertsState' <- newTVarIO $ AlertsWaiting alerts
      alertThread <- alertFetcher AlertFetcherState
                      { alertTorrentSession = weakSession,
                        alertState = alertsState',
                        alertWait = alertWait'
                      }
      finishCallback <- newEmptyMVar
      let looper =
            mainLoop'' alertsState' >> get >>= \case
              KillSyncThread callback -> void $ liftIO $ tryPutMVar finishCallback callback
              _ -> looper
          finisher = do
            setTorrentSessionActive session False
            count <- requestSaveTorrentResumeData session
            traceShowM torrState ("Collecting torrents", count)
            collectResumeDatas count
            finishCallback' <- tryTakeMVar finishCallback
            void $ maybe (return False) (`tryPutMVar` ()) finishCallback'
          collectResumeDatas 0 = trace torrState "Done, killing alert thread" $ killThread alertThread
          collectResumeDatas count = do
              alerts' <- atomically $ readAlert alertsState'
              andThen <- alerts' >>= (`withAlertPtr'` do
                  what <- alertWhat
                  case traceShow torrState ("Collecting", what) what of
                   "save_resume_data" -> do
                      torr <- alertTorrent
                      buff <- alertSaveResumeDataBuffer
                      case (torr, buff) of
                        (Just torr', Just buff') -> do
                          torrentHash <- liftIO $ getTorrentHash torr'
                          traceShowM torrState ("Saving torrent", torr', B.length buff')
                          liftIO $ B.writeFile (torrentResumeFile torrentHash) buff'
                          return $ collectResumeDatas $ count - 1
                        _ -> traceShowM torrState ("Got invalid save data") >> pure (pure ())
                   "save_resume_data_failed" -> traceM torrState "Save resume data failed" >> return $ collectResumeDatas $ count - 1
                   _ -> trace torrState ("Wrong alert " ++ what ++ ", retrying") $ return $ collectResumeDatas count)
              fromMaybe (collectResumeDatas count) andThen

      finally (evalStateT looper $ SyncThreadState empty empty empty) finisher
      where
        readAlert alertsState' = do
              s <- readTVar alertsState'
              case s of
                AlertsActive alerts -> return $ do
                  nextAlert' <- nextAlert alerts
                  case nextAlert' of
                    Just alert -> return $ Just alert
                    Nothing -> do
                      atomically $ writeTVar alertsState' (AlertsWaiting alerts)
                      return Nothing
                _ -> STM.retry
        readChans alertsState' = do
          cmd <- tryReadTChan chan
          case cmd of
            Just cmd' -> return $ (Right cmd')
            Nothing -> Left <$> readAlert alertsState'
        mainLoop'' :: TVar AlertListState -> StateT SyncState IO ()
        mainLoop'' alertsState' =
          liftIO (atomically $ readChans alertsState') >>= \case
           Right e -> case e of
            AddTorrent path torrentData -> do
              newTorrent <- liftIO $ addTorrent session torrentData $ torrentDataDir torrState
              case (newTorrent, path) of
                (Just newTorrent', Just path') -> do
                  state <- get
                  let state' = state {_newTorrentPath = alter (const $ Just path') newTorrent' $ state ^?! newTorrentPath}
                  put state'
                _ -> return ()
            OpenTorrent {SyncTypes._torrent = torrent, _fdCallback = callback, _piece = piece} -> do
              state <- get
              case state of
                KillSyncThread _ -> return ()
                SyncThreadState { _fdCursors = fds } -> do
                  torrentHash <- liftIO $ getTorrentHash torrent
                  let fdsForTorrent = fromMaybe empty $ lookup torrentHash $ fds
                      (newFdsForTorrent, fd) = insertFirstMap fdsForTorrent piece
                      state' = state {_fdCursors = alter (const $ Just newFdsForTorrent) torrentHash $ fds}
                  put state'
                  _ <- liftIO $ when (null fdsForTorrent) $ resetTorrent TorrentDownload session torrent >> pure ()
                  liftIO $ tryPutMVar callback fd
                  return ()
            CloseTorrent {SyncTypes._torrent = torrent, _fd = fd} -> do
              state <- get
              case state of
                KillSyncThread _ -> return ()
                SyncThreadState { _fdCursors = fds } -> do
                  torrentHash <- liftIO $ getTorrentHash torrent
                  traceShowM torrState ("Trying to close torrent", torrent, torrentHash, fd)
                  let fdsForTorrent = fromMaybe empty $ lookup torrentHash fds
                      newFdsForTorrent = delete fd fdsForTorrent
                      noFdsLeft = null newFdsForTorrent
                      newFdsForTorrent' = if noFdsLeft then Nothing else Just newFdsForTorrent
                      state' = state {_fdCursors = alter (const newFdsForTorrent') torrentHash fds}
                  put state'
                  traceShowM torrState ("Should reset?", noFdsLeft, fdsForTorrent, newFdsForTorrent')
                  when noFdsLeft $ void $ liftIO $ resetTorrent TorrentUploadOnly session torrent
            RemoveTorrent {SyncTypes._torrent = target} -> do
              state <- get
              ref <- liftIO $ deRefWeak (torrState ^. fuseFiles)
              torrentHash <- liftIO $ getTorrentHash target
              traceShowM torrState ("Trying to delete torrent", torrentHash)
              let filterTorrent x =
                    if x ^? TorrentFileSystem.hash == Just torrentHash
                      then Nothing
                      else case x ^? contents of
                        Just children -> Just $ x {_contents = Map.mapMaybe filterTorrent children}
                        Nothing -> Just x
              -- let fdsForTorrent = fromMaybe empty $ lookup torrentHash $ state^?!fds
              torrentRemoved <- liftIO $ removeTorrent session target
              traceShowM torrState ("Removal result", torrentRemoved)

              case ref of
                Just fs -> void $ liftIO $ atomically $ modifyTVar fs $ \x -> Map.mapMaybe filterTorrent x
                Nothing -> return ()
            ReadTorrent
              { SyncTypes._torrent = torrent,
                _fd = fd,
                _piece = piece,
                _pieceData = callbacks
              } -> do
                torrentHash <- liftIO $ getTorrentHash torrent
                traceM torrState $ "Adding " ++ show (length callbacks) ++ " read request for " ++ (show torrentHash)
                let newReads = [((torrentHash, piece+i), callback) | (i, callback) <- zip [0..] callbacks]
                    newFdPosition = piece + fromIntegral (length newReads) - 1
                oldState <- get
                case oldState of
                  KillSyncThread _ -> return ()
                  SyncThreadState { _inWait = inWait', _fdCursors = fds' } -> do
                    let -- newInWait :: Monad m => [((TorrentHash, TorrentPieceType), [TorrentReadCallback])] -> Map (TorrentHash, TorrentPieceType) [TorrentReadCallback] -> StateT [TorrentPieceType] m (Map (TorrentHash, TorrentPieceType) [TorrentReadCallback])
                        addCallbacks [] m = return m
                        addCallbacks ((key@(_, _), callback):xs) m = do
                          let updateCallbacks Nothing = do
                                put True
                                return $ Just [callback]
                              updateCallbacks (Just prev) = return $ Just $ callback : prev
                          m' <- alterF updateCallbacks key m
                          addCallbacks xs m'
                        addCallbacks' k m = runStateT (addCallbacks k m) False
                    (newInWait', newRequestedPieces) <- addCallbacks' newReads inWait'
                    let newFds = alter (maybe (Just $ Map.singleton fd newFdPosition) $ Just . Map.insert fd newFdPosition) torrentHash fds'
                        newState = oldState { _inWait = newInWait', _fdCursors = newFds }
                    put newState
                    when newRequestedPieces $ do
                      let requestedPieces = flip mapMaybe (keys $ newInWait') $
                            \(hash, piece) ->
                              if torrentHash == hash
                                then Just piece
                                else Nothing
                          fdPositions = maybe [] Map.elems (lookup torrentHash newFds)
                      void $ liftIO $ downloadTorrentParts session torrent (requestedPieces `union` fdPositions) 1000 25
            FuseDead mvar -> put $ KillSyncThread mvar
           Left alert -> do
              let publishTorrent torrent = do
                    ref <- liftIO $ deRefWeak (torrState ^. fuseFiles)
                    case ref of
                      Nothing -> return ()
                      Just fs -> do
                        state <- get
                        torrentHash <- liftIO $ getTorrentHash torrent
                        let (path, state') = Map.updateLookupWithKey (const $ const Nothing) torrentHash $ state ^?! newTorrentPath
                            createPath = fmap (flip pathToTFSDir) path
                        (name, filesystem) <- liftIO $ unpackTorrentFiles (_torrentTrace torrState) session torrent torrentHash
                        traceShowM torrState (name, filesystem, path, state', torrent, torrentHash)
                        unless (null filesystem) $ do
                          void $ liftIO $ do
                            newLostFiles <- atomically $ do
                              before <- readTVar fs
                              traceShowM torrState ("File system", before)
                              case createPath of
                                Just createPath' -> do
                                  writeTVar fs $ traceShowId torrState $ mergeDirectories2 before $ New $ createPath' filesystem
                                  return Nothing
                                Nothing -> do
                                  let (withMerged, New lost) = mergeDuplicatesFrom before (New filesystem)
                                  writeTVar fs $ traceShowId torrState withMerged
                                  return $ Just lost
                            let handleLost
                                  | Just x <- newLostFiles,
                                    not (null x),
                                    Just lostRef <- torrState ^. fuseLostFound = do
                                      lostRef' <- deRefWeak lostRef
                                      forM_
                                        lostRef'
                                        ( atomically
                                            . ( `modifyTVar`
                                                  \lost ->
                                                    (mergeDirectories2 lost (New x))
                                              )
                                        )
                                  | otherwise = return ()
                            handleLost
                          put $ state {_newTorrentPath = state'}
               in liftIO alert >>= (`withAlertPtr_` do
                 alertType' <- alertType
                 alertWhat' <- alertWhat
                 case traceShowId torrState (alertType', alertWhat') of
                    (5, "read_piece") -> alertTorrent >>= mapM_ (\torrent -> do
                      torrentHash <- liftIO $ getTorrentHash torrent
                      Just (piece, buf) <- alertReadPiece
                      let key = (torrentHash, piece)

                      state <- lift get
                      let (inWaitForPiece, newInWait) = partitionWithKey (\k _ -> k == key) $ state ^. inWait
                      liftIO $ forM_ inWaitForPiece $ mapM (`writeTorrentReadCallback` buf)
                      lift $ put $ state { _inWait = newInWait })
                    (67, "add_torrent") -> alertTorrent >>= mapM_ (lift . publishTorrent)
                    (45, "metadata_received") -> alertTorrent >>= mapM_ (lift . publishTorrent)
                    (43, "file_error") -> alertTorrent >>= \case
                      Just torrent -> liftIO $ checkTorrentHash session torrent
                      Nothing -> return ()
                    (4, "torrent_removed") -> traceShowM torrState ("Trying to delete", alertType') >> alertTorrentDeletedHash >>= mapM_ (\hash' -> do
                     traceShowM torrState ("Deleting resume file for torrent", hash', "with type", alertType')
                     liftIO $ flip catchIOError (const $ pure ()) $ removeFile $ torrentResumeFile hash')

                    (41, "torrent_checked") -> alertTorrent >>= \case
                      Nothing -> return ()
                      Just torrent -> do
                        state <- lift get
                        torrentHash <- liftIO $ getTorrentHash torrent
                        let pieces = flip mapMaybe (keys $ state ^. inWait) $ \(hash, piece) ->
                              if torrentHash == hash
                                then Just piece
                                else Nothing
                        void $ liftIO $ downloadTorrentParts session torrent pieces 1000 25
                    unknown -> traceShowM torrState ("Uknown alert", unknown))
