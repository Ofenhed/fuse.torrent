{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Torrent (withTorrentSession, addTorrent, waitForAlert, TorrentAlert(..), getTorrentFiles, getTorrentName, getTorrents, TorrentHandle(), TorrentInfo(..), TorrentFile(..)) where

import TorrentTypes
import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM, (>=>))
import Data.Data (Data(..), Typeable(..))

import Debug.Trace

#include "libtorrent_exports.h"
foreign import ccall "libtorrent_exports.h init_torrent_session" c_init_torrent_session :: CString -> IO TorrentSession
foreign import ccall "libtorrent_exports.h destroy_torrent_session" c_destroy_torrent_session :: CString -> TorrentSession -> IO ()

foreign import ccall "libtorrent_exports.h get_torrent_count" c_get_torrent_count :: TorrentSession -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent" c_unsafe_get_torrent :: TorrentSession -> CUInt -> IO (WithDestructor (Ptr CTorrentHandle))
foreign import ccall "libtorrent_exports.h add_torrent" c_unsafe_add_torrent :: TorrentSession -> CString -> CString -> IO (WithDestructor (Ptr CTorrentHandle))
foreign import ccall "libtorrent_exports.h get_torrent_name" c_get_torrent_name :: TorrentSession -> Ptr CTorrentHandle -> IO CString
foreign import ccall "libtorrent_exports.h torrent_has_metadata" c_torrent_has_metadata :: TorrentSession -> Ptr CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_num_files" c_get_torrent_num_files :: TorrentSession -> Ptr CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_info" c_unsafe_get_torrent_info :: TorrentSession -> Ptr CTorrentHandle -> IO (WithDestructor (Ptr TorrentInfo))

foreign import ccall "libtorrent_exports.h wait_for_alert" c_wait_for_alert :: TorrentSession -> Int -> IO Alert
foreign import ccall "libtorrent_exports.h get_alert_type" c_get_alert_type :: Alert -> IO Int
foreign import ccall "libtorrent_exports.h get_alert_what" c_get_alert_what :: Alert -> IO CString
foreign import ccall "libtorrent_exports.h get_alert_message" c_get_alert_msg :: Alert -> IO CString

c_get_torrent_info session torrent = c_unsafe_get_torrent_info session torrent >>= unpackFromDestructor
c_get_torrent session idx = c_unsafe_get_torrent session idx >>= unpackFromDestructor
c_add_torrent session hash path = c_unsafe_add_torrent session hash path >>= unpackFromDestructor

withTorrentSession :: String -> (TorrentSession -> IO a) -> IO a
withTorrentSession savefile runner = withCString savefile $ \cstring ->
  bracket (c_init_torrent_session cstring) (c_destroy_torrent_session cstring) runner

getTorrents :: TorrentSession -> IO [TorrentHandle]
getTorrents sess = do
  num_torrents <- c_get_torrent_count sess
  if num_torrents > 0
    then forM [0..num_torrents-1] $ c_get_torrent sess
    else return []

addTorrent :: TorrentSession -> String -> String -> IO TorrentHandle
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

getTorrentFiles :: TorrentSession -> TorrentHandle -> IO (Maybe TorrentInfo)
getTorrentFiles session torrent = do
  has_metadata <- withForeignPtr torrent $ c_torrent_has_metadata session
  if has_metadata == 0
    then return Nothing
    else
      withForeignPtr torrent $ \handle -> do
        info <- c_get_torrent_info session handle
        withForeignPtr info $ \info' ->
          if info' == nullPtr
             then return Nothing
             else peek info' >>= (return . Just . traceShowId)

getTorrentName :: TorrentSession -> TorrentHandle -> IO (Maybe String)
getTorrentName session torrent = do
  has_metadata <- withForeignPtr torrent $ c_torrent_has_metadata session
  if has_metadata == 0
    then return Nothing
    else do name <- withForeignPtr torrent $ c_get_torrent_name session
            if name == nullPtr
               then return Nothing
               else Just <$> peekCString name
