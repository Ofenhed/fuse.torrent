{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Torrent (withTorrentSession, addTorrent) where

import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.Marshal
import Foreign.Storable

#include "libtorrent_exports.h"
data TorrentSessionData = TorrentSessionData
type TorrentSession = Ptr TorrentSessionData
data Torrent = Torrent

foreign import ccall "libtorrent_exports.h init_torrent_session" c_init_torrent_session :: IO TorrentSession
foreign import ccall "libtorrent_exports.h destroy_torrent_session" c_destroy_torrent_session :: TorrentSession -> IO ()
foreign import ccall "libtorrent_exports.h add_torrent" c_add_torrent :: TorrentSession -> CString -> CString -> IO ()

withTorrentSession :: (TorrentSession -> IO a) -> IO a
withTorrentSession = bracket c_init_torrent_session c_destroy_torrent_session 

addTorrent :: TorrentSession -> String -> String -> IO (Maybe Torrent)
addTorrent session filename path = do
  withCString filename $ \filename ->
    withCString path $ \path -> c_add_torrent session filename path
  return $ Just Torrent
  
