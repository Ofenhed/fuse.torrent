{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Torrent (withTorrentSession, addTorrent, waitForAlert, TorrentAlert(..), getTorrentFiles, getTorrentName, getTorrents, TorrentHandle()) where

import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.Marshal
import Foreign.Storable

import Debug.Trace

#include "libtorrent_exports.h"
data CTorrentSession = CTorrentSession
type TorrentSession = Ptr CTorrentSession
data CTorrentHandle = CTorrentHandle
type TorrentHandle = Ptr CTorrentHandle
data CAlert = CAlert
type Alert = Ptr CAlert
data TorrentAlert = Alert { alertType :: Int, alertWhat :: String } deriving (Show)

foreign import ccall "libtorrent_exports.h init_torrent_session" c_init_torrent_session :: CString -> IO TorrentSession
foreign import ccall "libtorrent_exports.h destroy_torrent_session" c_destroy_torrent_session :: CString -> TorrentSession -> IO ()

foreign import ccall "libtorrent_exports.h get_torrent_count" c_get_torrent_count :: TorrentSession -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent" c_get_torrent :: TorrentSession -> CUInt -> IO TorrentHandle
foreign import ccall "libtorrent_exports.h add_torrent" c_add_torrent :: TorrentSession -> CString -> CString -> IO TorrentHandle
foreign import ccall "libtorrent_exports.h get_torrent_name" c_get_torrent_name :: TorrentSession -> TorrentHandle -> IO CString
foreign import ccall "libtorrent_exports.h torrent_has_metadata" c_torrent_has_metadata :: TorrentSession -> TorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_num_files" c_get_torrent_num_files :: TorrentSession -> TorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_file" c_get_torrent_file :: TorrentSession -> TorrentHandle -> CUInt -> IO CString

foreign import ccall "libtorrent_exports.h wait_for_alert" c_wait_for_alert :: TorrentSession -> Int -> IO (Alert)
foreign import ccall "libtorrent_exports.h get_alert_type" c_get_alert_type :: Alert -> IO (Int)
foreign import ccall "libtorrent_exports.h get_alert_what" c_get_alert_what :: Alert -> IO (CString)
foreign import ccall "libtorrent_exports.h get_alert_message" c_get_alert_msg :: Alert -> IO (CString)

withTorrentSession :: String -> (TorrentSession -> IO a) -> IO a
withTorrentSession savefile runner = withCString savefile $ \cstring ->
  bracket (c_init_torrent_session cstring) (c_destroy_torrent_session cstring) runner

getTorrents :: TorrentSession -> IO [TorrentHandle]
getTorrents sess = do
  num_torrents <- c_get_torrent_count sess
  traceShowM num_torrents
  if num_torrents > 0
    then flip mapM [0..num_torrents-1] $ c_get_torrent sess
    else return []

addTorrent :: TorrentSession -> String -> String -> IO (TorrentHandle)
addTorrent session filename path =
  withCString filename $ \filename ->
    withCString path $ \path -> c_add_torrent session filename path
  
-- NOT THREAD SAFE
waitForAlert :: TorrentSession -> Int -> IO (Maybe TorrentAlert)
waitForAlert session timeout = do
  alert <- c_wait_for_alert session timeout
  if alert == nullPtr
    then return Nothing
    else do
      aType <- c_get_alert_type alert
      aWhat <- c_get_alert_what alert >>= peekCString
      return $ Just $ Alert { alertType = aType, alertWhat = aWhat }

getTorrentFiles :: TorrentSession -> TorrentHandle -> IO (Maybe [String])
getTorrentFiles session torrent = do
  has_metadata <- c_torrent_has_metadata session torrent
  if has_metadata == 0
    then return Nothing
    else do
      num_files <- c_get_torrent_num_files session torrent
      if num_files == 0
        then return $ Just []
        else flip mapM [0..num_files-1] (\idx -> c_get_torrent_file session torrent idx >>= peekCString) >>= return . Just

getTorrentName :: TorrentSession -> TorrentHandle -> IO (Maybe String)
getTorrentName session torrent = do
  has_metadata <- c_torrent_has_metadata session torrent
  if has_metadata == 0
    then return Nothing
    else do
      c_get_torrent_name session torrent >>= peekCString >>= return . Just
