{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module OldTorrent (addTorrent, popAlert, TorrentAlert(..), getTorrentFiles, getTorrentName, getTorrents, resetTorrent, downloadTorrentParts, setTorrentSessionActive, resumeTorrent, checkTorrentHash, TorrentHandle(), TorrentInfo(..), TorrentFile(..), NewTorrentType(..)) where

import TorrentTypes
import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM, (>=>))
import Data.Data (Data(..), Typeable(..))
import Control.Concurrent.QSem (QSem, signalQSem)

import Data.ByteString as B

#include "libtorrent_exports.h"
foreign import ccall "libtorrent_exports.h init_torrent_session" c_init_torrent_session :: CString -> FunPtr (IO ()) -> IO CTorrentSession
foreign import ccall "libtorrent_exports.h destroy_torrent_session" c_destroy_torrent_session :: CString -> CTorrentSession -> IO ()
foreign import ccall "libtorrent_exports.h set_session_active" c_set_torrent_session_active :: CTorrentSession -> CUInt -> IO ()
foreign import ccall "libtorrent_exports.h save_torrents_resume_data" c_save_torrents_resume_data :: CTorrentSession -> IO CUInt

foreign import ccall "libtorrent_exports.h get_torrent_count" c_get_torrent_count :: CTorrentSession -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent" c_unsafe_get_torrent :: CTorrentSession -> CUInt -> IO (WithDestructor CTorrentHandle)
foreign import ccall "libtorrent_exports.h add_torrent_magnet" c_unsafe_add_torrent_magnet :: CTorrentSession -> CString -> CString -> IO (WithDestructor CTorrentHandle)
foreign import ccall "libtorrent_exports.h add_torrent_file" c_unsafe_add_torrent_file :: CTorrentSession -> CString -> CUInt -> CString -> IO (WithDestructor CTorrentHandle)
foreign import ccall "libtorrent_exports.h resume_torrent" c_unsafe_resume_torrent :: CTorrentSession -> CString -> CUInt -> CString -> IO (WithDestructor CTorrentHandle)
foreign import ccall "libtorrent_exports.h reset_torrent" c_reset_torrent :: CTorrentSession -> CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h check_torrent_hash" c_check_torrent_hash :: CTorrentSession -> CTorrentHandle -> IO ()
foreign import ccall "libtorrent_exports.h download_torrent_parts" c_download_torrent_parts :: CTorrentSession -> CTorrentHandle -> TorrentPieceType -> CUInt -> CUInt -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_name" c_get_torrent_name :: CTorrentSession -> CTorrentHandle -> IO CString
foreign import ccall "libtorrent_exports.h torrent_has_metadata" c_torrent_has_metadata :: CTorrentSession -> CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_num_files" c_get_torrent_num_files :: CTorrentSession -> CTorrentHandle -> IO CUInt
foreign import ccall "libtorrent_exports.h get_torrent_info" c_unsafe_get_torrent_info :: CTorrentSession -> CTorrentHandle -> IO (WithDestructor (Ptr TorrentInfo))

foreign import ccall "libtorrent_exports.h pop_alert" c_unsafe_pop_alert :: CTorrentSession -> IO (WithDestructor (Ptr TorrentAlert))
foreign import ccall "libtorrent_exports.h get_alert_type" c_get_alert_type :: Alert -> IO Int
foreign import ccall "libtorrent_exports.h get_alert_what" c_get_alert_what :: Alert -> IO CString
foreign import ccall "libtorrent_exports.h get_alert_message" c_get_alert_msg :: Alert -> IO CString
foreign import ccall "libtorrent_exports.h get_alert_torrent" c_unsafe_get_alert_torrent :: Alert -> IO (WithDestructor CTorrentHandle)

foreign import ccall "wrapper" c_wrap_callback :: IO () -> IO (FunPtr (IO ()))

c_get_torrent_info session torrent = c_unsafe_get_torrent_info session torrent >>= unpackFromDestructor
c_get_torrent session idx = c_unsafe_get_torrent session idx >>= unpackFromDestructor
c_add_torrent_magnet session hash path = c_unsafe_add_torrent_magnet session hash path >>= unpackFromMaybeDestructor
c_add_torrent_file session fileData fileLen path = c_unsafe_add_torrent_file session fileData fileLen path >>= unpackFromMaybeDestructor
c_resume_torrent session resumeData resumeLen path = c_unsafe_resume_torrent session resumeData resumeLen path >>= unpackFromMaybeDestructor
c_pop_alert session = c_unsafe_pop_alert session >>= unpackFromMaybeDestructor

getTorrents :: TorrentSession -> IO [TorrentHandle]
getTorrents sess = do
  num_torrents <- c_get_torrent_count $ torrentPointer sess
  if num_torrents > 0
    then forM [0..num_torrents-1] $ c_get_torrent (torrentPointer sess) >=> peekTorrent
    else return []

setTorrentSessionActive :: TorrentSession -> Bool -> IO ()
setTorrentSessionActive session active = c_set_torrent_session_active (torrentPointer session) (if active then 1 else 0)

addTorrent :: TorrentSession -> NewTorrentType -> FilePath -> IO (Maybe TorrentHandle)
addTorrent session (NewMagnetTorrent magnet) path =
  withCString magnet $ \magnet ->
    withCString path $ c_add_torrent_magnet (torrentPointer session) magnet >=> maybe (return Nothing) (peekTorrent >=> return . Just)

addTorrent session (NewTorrentFile file) path =
  B.useAsCStringLen file $ \(file, fileLen) ->
    withCString path $ c_add_torrent_file (torrentPointer session) file (fromIntegral fileLen) >=> maybe (return Nothing) (peekTorrent >=> return . Just)

resumeTorrent :: TorrentSession -> B.ByteString -> FilePath -> IO (Maybe TorrentHandle)
resumeTorrent session resumeData path =
  B.useAsCStringLen resumeData $ \(resumeData, resumeDataLen) ->
    withCString path $ c_resume_torrent (torrentPointer session) resumeData (fromIntegral resumeDataLen) >=> maybe (return Nothing) (peekTorrent >=> return . Just)

resetTorrent :: TorrentSession -> TorrentHandle -> IO Bool
resetTorrent session torrent = do
  result <- withTorrent torrent $ \t -> c_reset_torrent (torrentPointer session) t
  return $ result /= 0

checkTorrentHash :: TorrentSession -> TorrentHandle -> IO ()
checkTorrentHash session torrent = withTorrent torrent $ c_check_torrent_hash (torrentPointer session)

downloadTorrentParts :: TorrentSession -> TorrentHandle -> TorrentPieceType -> Word -> Word -> IO Bool
downloadTorrentParts session torrent part count timeout = do
  result <- withTorrent torrent $ \t -> c_download_torrent_parts (torrentPointer session) t part (fromIntegral count) $ fromIntegral timeout
  return $ result /= 0

popAlert :: TorrentSession -> IO (Maybe TorrentAlert)
popAlert session = do
  ptr <- c_pop_alert $ torrentPointer session
  maybe (return Nothing) (`withForeignPtr` (peek >=> return . Just)) ptr

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
