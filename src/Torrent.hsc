{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Torrent (withTorrentSession, addTorrent, popAlert, TorrentAlert(..), getTorrentFiles, getTorrentName, getTorrents, TorrentHandle(), TorrentInfo(..), TorrentFile(..)) where

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
import Control.Concurrent.QSem (QSem, signalQSem)

#include "libtorrent_exports.h"
foreign import ccall "libtorrent_exports.h init_torrent_session" c_init_torrent_session :: CString -> FunPtr (IO ()) -> IO CTorrentSession
foreign import ccall "libtorrent_exports.h destroy_torrent_session" c_destroy_torrent_session :: CString -> CTorrentSession -> IO ()

foreign import ccall "libtorrent_exports.h get_torrent_hash_len" c_get_torrent_hash_len :: CUInt
foreign import ccall "libtorrent_exports.h get_torrent_count" c_get_torrent_count :: CTorrentSession -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent" c_unsafe_get_torrent :: CTorrentSession -> CUInt -> IO (WithDestructor CTorrentHandle)
foreign import ccall "libtorrent_exports.h add_torrent" c_unsafe_add_torrent :: CTorrentSession -> CString -> CString -> IO (WithDestructor CTorrentHandle)
foreign import ccall "libtorrent_exports.h get_torrent_name" c_get_torrent_name :: CTorrentSession -> CTorrentHandle -> IO CString
foreign import ccall "libtorrent_exports.h torrent_has_metadata" c_torrent_has_metadata :: CTorrentSession -> CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_num_files" c_get_torrent_num_files :: CTorrentSession -> CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_info" c_unsafe_get_torrent_info :: CTorrentSession -> CTorrentHandle -> IO (WithDestructor (Ptr TorrentInfo))

foreign import ccall "libtorrent_exports.h pop_alert" c_pop_alert :: CTorrentSession -> IO Alert
foreign import ccall "libtorrent_exports.h get_alert_type" c_get_alert_type :: Alert -> IO Int
foreign import ccall "libtorrent_exports.h get_alert_what" c_get_alert_what :: Alert -> IO CString
foreign import ccall "libtorrent_exports.h get_alert_message" c_get_alert_msg :: Alert -> IO CString
foreign import ccall "libtorrent_exports.h get_alert_torrent" c_unsafe_get_alert_torrent :: Alert -> IO (WithDestructor CTorrentHandle)

foreign import ccall "wrapper" c_wrap_callback :: IO () -> IO (FunPtr (IO ()))

c_get_torrent_info session torrent = c_unsafe_get_torrent_info session torrent >>= unpackFromDestructor
c_get_torrent session idx = c_unsafe_get_torrent session idx >>= unpackFromDestructor
c_add_torrent session hash path = c_unsafe_add_torrent session hash path >>= unpackFromDestructor
c_get_alert_torrent alert = c_unsafe_get_alert_torrent alert >>= unpackFromMaybeDestructor

withTorrent :: TorrentHandle -> (CTorrentHandle -> IO a) -> IO a
withTorrent handle action = withCAStringLen handle (\(str, _) -> action str)

peekTorrent = flip withForeignPtr (\str -> peekCAStringLen (str, fromIntegral c_get_torrent_hash_len))

withTorrentSession :: String -> QSem -> (TorrentSession -> IO a) -> IO a
withTorrentSession savefile sem runner = withCString savefile $ \cstring -> do
  callback <- c_wrap_callback $ signalQSem sem
  bracket (TorrentSession <$> c_init_torrent_session cstring callback)
          (\ptr -> do
            c_destroy_torrent_session cstring $ torrentPointer ptr
            freeHaskellFunPtr callback) runner

getTorrents :: TorrentSession -> IO [TorrentHandle]
getTorrents sess = do
  num_torrents <- c_get_torrent_count $ torrentPointer sess
  if num_torrents > 0
    then forM [0..num_torrents-1] $ c_get_torrent (torrentPointer sess) >=> peekTorrent
    else return []

addTorrent :: TorrentSession -> String -> FilePath -> IO TorrentHandle
addTorrent session filename path =
  withCString filename $ \filename ->
    withCString path $ c_add_torrent (torrentPointer session) filename >=> peekTorrent

popAlert :: TorrentSession -> IO (Maybe TorrentAlert)
popAlert session = do
  alert <- c_pop_alert $ torrentPointer session
  if alert == nullPtr
    then return Nothing
    else do
      aType <- c_get_alert_type alert
      aWhat <- c_get_alert_what alert >>= peekCString
      torrent <- c_get_alert_torrent alert >>= \case
        Nothing -> return Nothing
        Just ptr -> Just <$> peekTorrent ptr
      return $ Just $ Alert { alertType = aType, alertWhat = aWhat, alertTorrent = torrent }

getTorrentFiles :: TorrentSession -> TorrentHandle -> IO (Maybe TorrentInfo)
getTorrentFiles session torrent = do
  has_metadata <- withTorrent torrent $ c_torrent_has_metadata $ torrentPointer session
  if has_metadata == 0
    then return Nothing
    else
      withTorrent torrent $ \handle -> do
        info <- c_get_torrent_info (torrentPointer session) handle
        withForeignPtr info $ \info' ->
          if info' == nullPtr
             then return Nothing
             else peek info' >>= (return . Just)

getTorrentName :: TorrentSession -> TorrentHandle -> IO (Maybe String)
getTorrentName session torrent = do
  has_metadata <- withTorrent torrent $ c_torrent_has_metadata $ torrentPointer session
  if has_metadata == 0
    then return Nothing
    else do name <- withTorrent torrent $ c_get_torrent_name $ torrentPointer session
            if name == nullPtr
               then return Nothing
               else Just <$> peekCString name
