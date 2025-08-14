{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SyncTypes where

import Control.Concurrent (MVar)
import Data.Map.Strict (Map)
import Control.Lens (makeLenses)
import System.Mem.Weak (Weak, deRefWeak)
import Data.Set (Set)

import qualified Data.ByteString as B

import TorrentFileSystem (TorrentFileSystemEntryList, TorrentFd)

import TorrentTypes
import System.Posix (Fd)
import GHC.Conc (TVar)
import Control.Concurrent.STM (TChan, newEmptyTMVarIO, atomically, takeTMVar, tryPutTMVar, STM)
import Control.Concurrent.STM.TMVar (TMVar, mkWeakTMVar)
import Control.Monad (void)
import Utils (OptionalDebug(..), OptionalTrace(..), WithDebug)

type TorrentReadCallback = Weak (TMVar B.ByteString)
type TorrentReadReply = TMVar B.ByteString

newTorrentReadCallback :: IO (TorrentReadReply, TorrentReadCallback)
newTorrentReadCallback = do
  var <- newEmptyTMVarIO
  wvar <- mkWeakTMVar var $ pure ()
  return (var, wvar)

readTorrentReadCallback :: TorrentReadReply -> STM B.ByteString
readTorrentReadCallback = takeTMVar

writeTorrentReadCallback :: TorrentReadCallback -> B.ByteString -> IO ()
writeTorrentReadCallback cb b = do
  cb' <- deRefWeak cb
  atomically $ mapM_ (`tryPutTMVar` b) cb'

data SyncEvent = AddTorrent (Maybe FilePath) NewTorrentType
               | FuseDead (MVar ())
               | OpenTorrent { _torrent :: TorrentHandle
                             , _piece :: TorrentPieceType
                             , _fdCallback :: MVar TorrentFd }
               | CloseTorrent { _torrent :: TorrentHandle
                              , _fd :: TorrentFd }
               | ReadTorrent { _torrent :: TorrentHandle
                                    , _fd :: TorrentFd
                                    , _piece :: TorrentPieceType
                                    , _count :: Word
                                    , _pieceData :: [TorrentReadCallback] }
               | RemoveTorrent { _torrent :: TorrentHandle }
makeLenses ''SyncEvent

data FuseState where
  FuseState :: { _files :: TVar TorrentFileSystemEntryList, _hiddenDirs :: [FilePath], _syncChannel :: TChan SyncEvent, _newFiles :: TVar (Set FilePath), _realStatePath :: (Fd, FilePath), _lostFound :: Maybe (TVar TorrentFileSystemEntryList), _nonBlockTimeout :: Word, _fuseTrace :: OptionalTrace } -> FuseState

instance OptionalDebug FuseState OptionalTrace where
  type WithDebug FuseState = OptionalTrace
  traceObject = _fuseTrace

data TorrentState = TorrentState { _fuseFiles :: Weak (TVar TorrentFileSystemEntryList), _statePath :: FilePath, _fuseLostFound :: Maybe (Weak (TVar TorrentFileSystemEntryList)), _torrentTrace :: OptionalTrace }
makeLenses ''TorrentState

instance OptionalDebug TorrentState OptionalTrace where
  type WithDebug TorrentState = OptionalTrace
  traceObject = _torrentTrace

type FdList = Map TorrentHash (Map TorrentFd TorrentPieceType)

data SyncState = SyncThreadState { _inWait :: Map (TorrentHash, TorrentPieceType) [TorrentReadCallback], _fdCursors :: FdList, _newTorrentPath :: Map TorrentHash FilePath }
               | KillSyncThread (MVar ())
makeLenses ''SyncState
