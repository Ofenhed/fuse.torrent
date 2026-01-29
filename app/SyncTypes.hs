{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SyncTypes where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (STM, TChan, atomically, newEmptyTMVarIO, takeTMVar, tryPutTMVar)
import Control.Concurrent.STM.TMVar (TMVar, mkWeakTMVar)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Conc (TVar)
import System.LibFuse3 (FuseConfig)
import System.Mem.Weak (Weak, deRefWeak)
import System.Posix (Fd)
import qualified TorrentFileSystem as TFS
import TorrentTypes
import Utils (AlwaysEq (AlwaysEq), OptionalDebug (..), OptionalTrace (..), WithDebug)

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

type TorrentFd = TFS.TorrentFd

data SyncEvent
  = AddTorrent (Maybe FilePath) NewTorrentType
  | FuseDead (MVar ())
  | OpenTorrent
      { _torrent :: TorrentHandle,
        _piece :: TorrentPieceType,
        _fdCallback :: MVar TorrentFd
      }
  | CloseTorrent
      { _torrent :: TorrentHandle,
        _fd :: TorrentFd
      }
  | ReadTorrent
      { _torrent :: TorrentHandle,
        _fd :: TorrentFd,
        _piece :: TorrentPieceType,
        _pieceData :: [TorrentReadCallback]
      }
  | RemoveTorrent {_torrent :: TorrentHandle}

makeLenses ''SyncEvent

data FileAttributes = FileAttributes deriving (Show, Read, Eq)

instance TFS.DefaultAttributes (AlwaysEq FileAttributes) where
  defaultFileAttrs = AlwaysEq FileAttributes
  defaultDirAttrs = AlwaysEq FileAttributes

type TorrentFileSystemEntry = TFS.TorrentFileSystemEntry (AlwaysEq FileAttributes)

type TorrentFileSystemEntry' = TorrentFileSystemEntry

type TorrentFileSystemEntryList = TFS.TorrentFileSystemEntryList (AlwaysEq FileAttributes)

type TorrentFileSystemEntryList' ba = TFS.TorrentFileSystemEntryList' (AlwaysEq FileAttributes) ba

type TorrentFileSystemEntryList'' = TorrentFileSystemEntryList

type TFSHandle = TFS.TFSHandle (AlwaysEq FileAttributes)

type TFSHandle' = TFSHandle

data FuseState where
  FuseState :: {_files :: TVar TorrentFileSystemEntryList, _hiddenDirs :: [FilePath], _syncChannel :: TChan SyncEvent, _newFiles :: TVar (Set FilePath), _realStatePath :: (Fd, FilePath), _lostFound :: Maybe (TVar TorrentFileSystemEntryList), _nonBlockTimeout :: Word, _ignoreNonBlock :: Bool, _cachedBlocks :: Word, _fuseTrace :: OptionalTrace, _fuseConfig :: TVar (Maybe FuseConfig)} -> FuseState

instance OptionalDebug FuseState OptionalTrace where
  type WithDebug FuseState = OptionalTrace
  traceObject = _fuseTrace

data TorrentState = TorrentState {_fuseFiles :: Weak (TVar TorrentFileSystemEntryList), _statePath :: FilePath, _fuseLostFound :: Maybe (Weak (TVar TorrentFileSystemEntryList)), _torrentTrace :: OptionalTrace}

makeLenses ''TorrentState

instance OptionalDebug TorrentState OptionalTrace where
  type WithDebug TorrentState = OptionalTrace
  traceObject = _torrentTrace

type FdList = Map TorrentHash (Map TorrentFd TorrentPieceType)

data SyncState
  = SyncThreadState {_inWait :: Map (TorrentHash, TorrentPieceType) [TorrentReadCallback], _fdCursors :: FdList, _newTorrentPath :: Map TorrentHash FilePath}
  | KillSyncThread (MVar ())

makeLenses ''SyncState
