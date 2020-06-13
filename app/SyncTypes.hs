{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SyncTypes where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent (MVar)
import Data.Map.Strict (Map)
import Control.Lens (makeLenses)
import System.Mem.Weak (Weak)
import Data.IORef (IORef)

import qualified Data.ByteString as B

import TorrentFileSystem (TorrentFileSystemEntryList)

import TorrentTypes

data SyncEvent = NewAlert TorrentAlert
               | AddTorrent String
               | FuseDead (MVar Bool)
               | RequestStartTorrent { _torrent :: TorrentHandle
                                     , _fd :: MVar Word }
               | RequestFileContent { _torrent :: TorrentHandle
                                    , _piece :: TorrentPieceType
                                    , _count :: Word
                                    , _fileData :: MVar B.ByteString }
makeLenses ''SyncEvent

data FuseState = FuseState { _files :: IORef TorrentFileSystemEntryList, _hiddenDirs :: [FilePath], _syncChannel :: Chan SyncEvent }
makeLenses ''FuseState
data TorrentState = TorrentState { _fuseFiles :: Weak (IORef TorrentFileSystemEntryList), _statePath :: FilePath }
makeLenses ''TorrentState

data SyncState = SyncThreadState { _inWait :: Map (TorrentHandle, TorrentPieceSizeType) [MVar B.ByteString]}
               | KillSyncThread (MVar Bool)
makeLenses ''SyncState
