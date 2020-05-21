{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Torrent (withTorrentSession, addTorrent, waitForAlert, TorrentAlert(..), getTorrentFiles, getTorrentName, getTorrents, TorrentHandle()) where

import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM)

#include "libtorrent_exports.h"
data CTorrentSession = CTorrentSession
type TorrentSession = Ptr CTorrentSession
data CWithDestructor a = CWithDestructor (Ptr()) (Ptr a)
type WithDestructor a = Ptr (CWithDestructor a)
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
foreign import ccall "libtorrent_exports.h get_torrent_files" c_unsafe_get_torrent_files :: TorrentSession -> TorrentHandle -> IO (WithDestructor CString)

foreign import ccall "libtorrent_exports.h wait_for_alert" c_wait_for_alert :: TorrentSession -> Int -> IO Alert
foreign import ccall "libtorrent_exports.h get_alert_type" c_get_alert_type :: Alert -> IO Int
foreign import ccall "libtorrent_exports.h get_alert_what" c_get_alert_what :: Alert -> IO CString
foreign import ccall "libtorrent_exports.h get_alert_message" c_get_alert_msg :: Alert -> IO CString

foreign import ccall "libtorrent_exports.h &delete_object_with_destructor" p_delete_object_with_destructor :: FinalizerEnvPtr (CWithDestructor a) a

unpackFromDestructor :: Storable a => WithDestructor a -> IO (ForeignPtr a)
unpackFromDestructor ptr = do
  CWithDestructor _ object <- peek ptr
  newForeignPtrEnv p_delete_object_with_destructor ptr object

  

c_get_torrent_files session torrent = c_unsafe_get_torrent_files session torrent >>= unpackFromDestructor

instance Storable stored => Storable (CWithDestructor stored) where
  alignment _ = #{alignment h_with_destructor}
  sizeOf _ = #{size h_with_destructor}
  peek ptr = do
    destructor <- #{peek h_with_destructor, destructor} ptr
    object <- #{peek h_with_destructor, object} ptr
    return (CWithDestructor destructor object)
  poke ptr (CWithDestructor destructor object) = do
    #{poke h_with_destructor, destructor} ptr destructor
    #{poke h_with_destructor, object} ptr object

unpackArrayPtr :: Storable a => (a -> IO (Maybe b)) -> Ptr a -> IO (Maybe [b])
unpackArrayPtr unpack a = if a == nullPtr
                      then return $ Nothing
                      else Just <$> fetchUntilNull 0
  where
    fetchUntilNull idx = do
      curr <- peekElemOff a idx
      curr' <- unpack curr
      case curr' of
        Nothing -> return []
        Just curr'' -> do
          rest <- fetchUntilNull $ idx + 1
          return $ curr'':rest

unpackStringArray = unpackArrayPtr (\ptr -> if ptr == nullPtr then return Nothing else Just <$> peekCString ptr)

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

getTorrentFiles :: TorrentSession -> TorrentHandle -> IO (Maybe [String])
getTorrentFiles session torrent = do
  has_metadata <- c_torrent_has_metadata session torrent
  if has_metadata == 0
    then return Nothing
    else do
      v <- c_get_torrent_files session torrent
      flip withForeignPtr unpackStringArray v

getTorrentName :: TorrentSession -> TorrentHandle -> IO (Maybe String)
getTorrentName session torrent = do
  has_metadata <- c_torrent_has_metadata session torrent
  if has_metadata == 0
    then return Nothing
    else do name <- c_get_torrent_name session torrent
            if name == nullPtr
               then return Nothing
               else Just <$> peekCString name
