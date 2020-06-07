{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SyncTypes where

import Control.Concurrent.QSem (QSem)
import Control.Concurrent (Chan)
import Control.Lens
import System.Mem.Weak (Weak)
import Data.IORef (IORef)

import TorrentFileSystem (TorrentFileSystemEntryList)

import TorrentTypes

data SyncEvent = NewAlert TorrentAlert
               | AddTorrent String FilePath
               | FuseDead
               | RequestStartTorrent { _torrent :: TorrentHandle, _callback :: QSem }
               | RequestFileContent { _torrent :: TorrentHandle
                                    , _piece :: TorrentPieceType
                                    , _count :: Word
                                    , _callback :: QSem }
makeLenses ''SyncEvent

data FuseState = FuseState { _files :: IORef TorrentFileSystemEntryList, _syncChannel :: Chan SyncEvent }
makeLenses ''FuseState
newtype TorrentState = TorrentState { _fuseFiles :: Weak (IORef TorrentFileSystemEntryList) }
makeLenses ''TorrentState

