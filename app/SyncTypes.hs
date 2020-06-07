{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SyncTypes where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent (Chan)
import Data.Map.Strict (Map)
import Control.Lens (makeLenses)
import System.Mem.Weak (Weak)
import Data.IORef (IORef)

import qualified Data.ByteString as B

import TorrentFileSystem (TorrentFileSystemEntryList)

import TorrentTypes

data SyncEvent = NewAlert TorrentAlert
               | AddTorrent String FilePath
               | FuseDead
               | RequestStartTorrent { _torrent :: TorrentHandle }
               | RequestFileContent { _torrent :: TorrentHandle
                                    , _piece :: TorrentPieceType
                                    , _count :: Word
                                    , _callback :: Chan B.ByteString }
makeLenses ''SyncEvent

data FuseState = FuseState { _files :: IORef TorrentFileSystemEntryList, _syncChannel :: Chan SyncEvent }
makeLenses ''FuseState
newtype TorrentState = TorrentState { _fuseFiles :: Weak (IORef TorrentFileSystemEntryList) }
makeLenses ''TorrentState

data SyncState = SyncThreadState { _inWait :: Map (TorrentHandle, TorrentPieceSizeType) [Chan B.ByteString]}
               | KillSyncThread
makeLenses ''SyncState
