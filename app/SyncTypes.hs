{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SyncTypes where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent (MVar)
import Data.Map.Strict (Map)
import Control.Lens (makeLenses)
import System.Mem.Weak (Weak)
import Data.IORef (IORef)
import Data.Set (Set)

import qualified Data.ByteString as B

import TorrentFileSystem (TorrentFileSystemEntryList, TorrentFd)

import TorrentTypes

data SyncEvent = NewAlert TorrentAlert
               | AddTorrent NewTorrentType
               | FuseDead (MVar ())
               | RequestStartTorrent { _torrent :: TorrentHandle
                                     , _fdCallback :: MVar TorrentFd }
               | CloseTorrent { _torrent :: TorrentHandle
                              , _fd :: TorrentFd }
               | RequestFileContent { _torrent :: TorrentHandle
                                    , _piece :: TorrentPieceType
                                    , _count :: Word
                                    , _fileData :: MVar B.ByteString }
               | RemoveTorrent { _torrent :: TorrentHandle }
makeLenses ''SyncEvent

data FuseState = FuseState { _files :: IORef TorrentFileSystemEntryList, _hiddenDirs :: [FilePath], _syncChannel :: Chan SyncEvent, _newFiles :: IORef (Set FilePath) }
makeLenses ''FuseState
data TorrentState = TorrentState { _fuseFiles :: Weak (IORef TorrentFileSystemEntryList), _statePath :: FilePath }
makeLenses ''TorrentState

data SyncState = SyncThreadState { _inWait :: Map (TorrentHandle, TorrentPieceSizeType) [MVar B.ByteString], _fds :: Map TorrentHandle (Map TorrentFd TorrentPieceType) }
               | KillSyncThread (MVar ())
makeLenses ''SyncState
