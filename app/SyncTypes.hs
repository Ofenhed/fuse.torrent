{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SyncTypes where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent (MVar)
import Data.Map.Strict (Map)
import Control.Lens (makeLenses)
import System.Mem.Weak (Weak)
import Data.Set (Set)

import qualified Data.ByteString as B

import TorrentFileSystem (TorrentFileSystemEntryList, TorrentFd)

import TorrentTypes
import System.Posix (Fd)
import GHC.Conc (TVar)

data SyncEvent = NewAlert TorrentAlert
               | AddTorrent (Maybe FilePath) NewTorrentType
               | FuseDead (MVar ())
               | OpenTorrent { _torrent :: TorrentHandle
                             , _piece :: TorrentPieceType
                             , _fdCallback :: MVar TorrentFd }
               | CloseTorrent { _torrent :: TorrentHandle
                              , _fd :: TorrentFd }
               | ReadTorrent { _torrent :: TorrentHandle
                                    , _fd :: TorrentFd
                                    , _piece :: TorrentPieceType
                                    , _fileData :: [MVar B.ByteString] }
               | RemoveTorrent { _torrent :: TorrentHandle }
makeLenses ''SyncEvent

data FuseState = FuseState { _files :: TVar TorrentFileSystemEntryList, _hiddenDirs :: [FilePath], _syncChannel :: Chan SyncEvent, _newFiles :: TVar (Set FilePath), _realStatePath :: (Fd, FilePath), _lostFound :: Maybe (TVar TorrentFileSystemEntryList) }
makeLenses ''FuseState
data TorrentState = TorrentState { _fuseFiles :: Weak (TVar TorrentFileSystemEntryList), _statePath :: FilePath, _fuseLostFound :: Maybe (Weak (TVar TorrentFileSystemEntryList)) }
makeLenses ''TorrentState

type FdList = Map TorrentHash (Map TorrentFd TorrentPieceType)

data SyncState = SyncThreadState { _inWait :: Map (TorrentHash, TorrentPieceSizeType) [MVar B.ByteString], _fds :: FdList, _newTorrentPath :: Map TorrentHash FilePath }
               | KillSyncThread (MVar ())
makeLenses ''SyncState
