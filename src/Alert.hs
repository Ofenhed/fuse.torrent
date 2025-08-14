{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Alert (Alerts, newAlertContainer, popAlerts, peekAlert, nextAlert, AlertMonad, withAlertPtr, alertTorrent, alertTorrentDeletedHash, alertReadPiece, alertCategory, alertWhat, alertType, alertErrorMsg, alertSaveResumeDataBuffer, withAlertPtr_, withAlertPtr', withAlertPtr_') where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, swapTVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.State.Strict as SM
import Control.Monad.Trans (MonadTrans)
import qualified Data.ByteString as B
import Data.Default (Default (def))
import Foreign (Ptr, alloca, nullPtr, peek)
import Foreign.C.String (peekCString)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, touchForeignPtr, withForeignPtr)
import InlineTypes
import IntoOwned (IntoOwned (intoOwned), PtrIntoForeignPtr (destructor))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import TorrentContext (torrentContext)
import TorrentTypes
  ( LibTorrentAlert,
    TorrentHandle,
    TorrentPieceType,
    TorrentSession,
  )
import TorrentUtils (getBestHash)
import Utils (IntoByteString (..))

C.context $ torrentContext
C.include "<iostream>"
C.include "libtorrent/alert_types.hpp"
C.include "libtorrent/write_resume_data.hpp"
C.include "libtorrent/session.hpp"

data CurrentAlert
  = NewAlert
  | AlertIndex C.CSize (ForeignPtr LibTorrentAlert)
  | EndOfAlerts

data AlertState = AlertState
  { arrCurr :: CurrentAlert,
    arrSize :: C.CSize
  }

type AlertVector = StdVector (Ptr LibTorrentAlert)

type SingleAlert = ForeignPtr LibTorrentAlert

type SingleWeakAlert = Weak SingleAlert

instance Default AlertState where
  def = AlertState {arrCurr = NewAlert, arrSize = 0}

instance PtrIntoForeignPtr AlertVector where
  destructor ptr = [CU.exp| void { delete $(std::vector<lt::alert*>* ptr) } |]

data Alerts = Alerts
  { alertsVec :: ForeignPtr AlertVector,
    alertState :: TVar AlertState
  }

newAlertContainer :: IO Alerts
newAlertContainer = do
  v <- [CU.exp| std::vector<lt::alert*>* {new std::vector<lt::alert*>()}|] >>= intoOwned
  state <- newTVarIO def
  return Alerts {alertsVec = v, alertState = state}

alertIdx :: Ptr (AlertVector) -> C.CSize -> Ptr LibTorrentAlert
alertIdx v idx = [CU.pure| lt::alert* { (*$(std::vector<lt::alert*>* v))[$(size_t idx)] } |]

atomicallySetCurrentAlert :: Alerts -> (C.CSize, Ptr LibTorrentAlert) -> IO (ForeignPtr LibTorrentAlert)
atomicallySetCurrentAlert Alerts {alertsVec = v, alertState = s} (idx, p) = do
  p' <- newForeignPtr p (touchForeignPtr v)
  atomically $ modifyTVar' s $ \x -> x {arrCurr = AlertIndex idx p'}
  return $ p'

popAlerts :: TorrentSession -> Alerts -> IO Int
popAlerts session Alerts {alertsVec = v, alertState = s} = do
  old <- atomically $ swapTVar s def
  case arrCurr old of
    AlertIndex _ ptr -> finalizeForeignPtr ptr
    _ -> pure ()
  withForeignPtr v $ \v' -> do
    size' <-
      [CU.block| size_t {
        auto vec = $(std::vector<lt::alert*>* v');
        $(lt::session* session)->pop_alerts(vec);
        return vec->size();
    } |]
    when (size' > 0) $ do
      void $ atomically $ modifyTVar' s $ \s' -> s' {arrSize = size'}
    return $ fromIntegral size'

peekAlert :: Alerts -> IO (Maybe SingleWeakAlert)
peekAlert Alerts {alertState = s} = do
  AlertState {arrCurr = curr'} <- readTVarIO s
  case curr' of
    AlertIndex _ p -> Just <$> mkWeakPtr p Nothing
    _ -> return Nothing

nextAlert :: Alerts -> IO (Maybe SingleWeakAlert)
nextAlert a@Alerts {alertsVec = v, alertState = s} = do
  action <- atomically $ do
    AlertState {arrCurr = curr', arrSize = size} <- readTVar s

    let destroyPrev p = finalizeForeignPtr p
        action
          | AlertIndex idx p <- curr',
            nextIdx <- idx + 1,
            nextIdx < size = do
              destroyPrev p
              nextP <- withForeignPtr v $ pure . (`alertIdx` nextIdx)
              nextP' <- atomicallySetCurrentAlert a (nextIdx, nextP)
              Just <$> mkWeakPtr nextP' Nothing
          | NewAlert <- curr',
            nextIdx <- 0,
            nextIdx < size = do
              nextP <- withForeignPtr v $ pure . (`alertIdx` nextIdx)
              nextP' <- atomicallySetCurrentAlert a (nextIdx, nextP)
              Just <$> mkWeakPtr nextP' Nothing
          | AlertIndex _ p <- curr' = do
              destroyPrev p
              atomically $ modifyTVar' s $ \x -> x {arrCurr = EndOfAlerts}
              return Nothing
          | otherwise = return Nothing
    return action
  action

withAlertPtr' :: (MonadIO m) => Maybe SingleWeakAlert -> AlertMonad m a -> m (Maybe a)
withAlertPtr' (Just weak') f = do
  ptr' <- SM.liftIO $ deRefWeak weak'
  case ptr' of
    Just fp -> do
      r <- SM.liftIO $ withForeignPtr fp (pure <$> withPtr' f)
      Just <$> r
    Nothing -> pure Nothing
withAlertPtr' Nothing _ = return Nothing

withAlertPtr :: (MonadIO m) => SingleWeakAlert -> AlertMonad m a -> m (Maybe a)
withAlertPtr a = withAlertPtr' $ Just a

withAlertPtr_ :: (MonadIO m) => Maybe (SingleWeakAlert) -> AlertMonad m a -> m ()
withAlertPtr_ weak' f = void $ withAlertPtr' weak' f

withAlertPtr_' :: (MonadIO m) => SingleWeakAlert -> AlertMonad m a -> m ()
withAlertPtr_' weak' f = void $ withAlertPtr weak' f

data AlertMonad m a = AlertMonad {withPtr' :: Ptr LibTorrentAlert -> m a}

instance (Monad m) => Monad (AlertMonad m) where
  m >>= f = AlertMonad $ \s -> do
    a <- withPtr' m s
    withPtr' (f a) s

instance (Functor m) => Functor (AlertMonad m) where
  fmap f m = AlertMonad $ \s ->
    let a = withPtr' m s
     in fmap f a

instance (Functor m, Monad m) => Applicative (AlertMonad m) where
  pure a = AlertMonad $ \_ -> pure a
  liftA2 f m1 m2 = AlertMonad $ \s -> do
    a <- withPtr' m1 s
    b <- withPtr' m2 s
    pure (f a b)

instance (MonadIO m) => MonadIO (AlertMonad m) where
  liftIO a = AlertMonad $ \_ -> SM.liftIO a

instance MonadTrans AlertMonad where
  lift m = AlertMonad $ \_ ->
    do
      a <- m
      return a

instance (MonadFail m) => MonadFail (AlertMonad m) where
  fail str = AlertMonad $ \_ -> fail str

getPtr :: (Applicative m) => AlertMonad m (Ptr LibTorrentAlert)
getPtr = AlertMonad $ \s -> pure s

alertType :: (Monad m) => AlertMonad m C.CInt
alertType = getPtr >>= \p -> return [CU.pure| int { $(lt::alert* p)->type() } |]

alertWhat :: (MonadIO m) => AlertMonad m String
alertWhat = getPtr >>= \p -> liftIO $ peekCString [CU.pure| char const* { $(lt::alert* p)->what() } |]

alertCategory :: (Monad m) => AlertMonad m C.CInt
alertCategory = getPtr >>= \p -> return [CU.pure| int { $(lt::alert* p)->category() } |]

alertReadPiece :: (MonadIO m) => AlertMonad m (Maybe (TorrentPieceType, B.ByteString))
alertReadPiece = do
  p <- getPtr
  liftIO $ alloca $ \size -> alloca $ \piece -> alloca $ \buf -> do
    arr <-
      [CU.block| boost::shared_array<char>* {
        if (auto read_piece_alert = lt::alert_cast<lt::read_piece_alert>($(lt::alert* p))) {
            auto arr = new boost::shared_array<char>(read_piece_alert->buffer);
            *$(int* piece) = read_piece_alert->piece;
            *$(size_t* size) = read_piece_alert->size;
            *$(char** buf) = arr->get();
            return arr;
        } else {
            return NULL;
        }
    } |]
    if buf == nullPtr
      then return Nothing
      else do
        size' <- peek size
        piece' <- peek piece
        buf' <- peek buf
        str <- intoOwned $ FromBoostSharedArray arr size' $ Just buf'
        return $ Just (fromIntegral piece', str)

alertSaveResumeDataBuffer :: (MonadIO m) => AlertMonad m (Maybe (B.ByteString))
alertSaveResumeDataBuffer = do
  p <- getPtr
  liftIO $ alloca $ \size -> alloca $ \buf -> do
    vec <-
      [CU.block| std::vector<char>* {
        if (auto save_resume_data_alert = lt::alert_cast<lt::save_resume_data_alert>($(lt::alert* p))) {
            auto vec = new std::vector<char>(lt::write_resume_data_buf(save_resume_data_alert->params));
            *$(char** buf) = vec->data();
            *$(size_t* size) = vec->size();
            return vec;
        } else {
            return NULL;
        }
    } |]
    if buf == nullPtr
      then return Nothing
      else do
        size' <- peek size
        buf' <- peek buf
        str <- intoOwned $ FromStdVec vec (Just size') (Just buf')
        return $ Just str

alertTorrent :: (MonadIO m) => AlertMonad m (Maybe TorrentHandle)
alertTorrent = do
  p <- getPtr
  handle <-
    liftIO
      [CU.block| lt::torrent_handle* {
      if (auto torrent_alert = dynamic_cast<lt::torrent_alert*>($(lt::alert* p))) {
          return new lt::torrent_handle(torrent_alert->handle);
      } else {
          return NULL;
      }
  } |]
  if handle == nullPtr
    then return Nothing
    else Just <$> liftIO (intoOwned handle)

alertTorrentDeletedHash :: (MonadIO m) => AlertMonad m (Maybe B.ByteString)
alertTorrentDeletedHash = do
  p <- getPtr
  let handler =
        [C.funPtr| bool alert_hash(lt::alert *alert, lt::info_hash_t* into) {
            if (auto deleted = dynamic_cast<lt::torrent_deleted_alert*>(alert)) {
                *into = deleted->info_hashes;
                return true;
            } else if (auto removed = dynamic_cast<lt::torrent_removed_alert*>(alert)) {
                *into = removed->info_hashes;
                return true;
            } else {
                return false;
            }
      } |]
  liftIO (getBestHash handler p)

alertErrorMsg :: (MonadIO m) => AlertMonad m (Maybe B.ByteString)
alertErrorMsg = do
  p <- getPtr
  msg <-
    liftIO
      [CU.block| std::string* {
      if (auto torrent_alert = dynamic_cast<lt::torrent_alert*>($(lt::alert* p))) {
          return new std::string(std::move(torrent_alert->message()));
      } else {
          return NULL;
      }
  } |]
  if msg == nullPtr
    then return Nothing
    else Just <$> (liftIO $ intoOwned $ FromStdString msg Nothing Nothing)

-- withAlert :: Monad m => SingleAlert -> StateT SingleAlert m
-- withAlert =
