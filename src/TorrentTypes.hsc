{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module TorrentTypes where

import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM)
import Data.Data (Data(..), Typeable(..))
import Control.Lens

import qualified Data.ByteString as B

import Debug.Trace

#include "libtorrent_exports.h"
data ValuelessPointer = ValuelessPointer deriving Eq
newtype CWithDestructor a = CWithDestructor a
type WithDestructor a = Ptr (CWithDestructor a)

type TorrentPieceType = Word
type TorrentPieceOffsetType = Word
type TorrentPieceSizeType = Word

type CTorrentSession = Ptr ValuelessPointer
data TorrentSession = TorrentSession CTorrentSession
type CTorrentHandle = CString
type TorrentHandle = String
data TorrentFile = TorrentFile { _filename :: FilePath
                               , _pieceStart :: TorrentPieceType
                               , _pieceStartOffset :: TorrentPieceOffsetType
                               , _filesize :: Word } deriving Show
makeLenses ''TorrentFile
data TorrentInfo = TorrentInfo { _torrentFiles :: [TorrentFile]
                               , _pieceSize :: TorrentPieceSizeType
                               , _filesPath :: FilePath } deriving Show
makeLenses ''TorrentInfo
data CAlert = CAlert
type Alert = Ptr CAlert
data TorrentAlert = Alert { _alertType :: Int
                          , _alertWhat :: String
                          , _alertCategory :: Int
                          , _alertTorrent :: Maybe TorrentHandle
                          , _alertPiece :: TorrentPieceType
                          , _alertBuffer :: Maybe B.ByteString } deriving (Show)
makeLenses ''TorrentAlert

foreign import ccall "libtorrent_exports.h get_torrent_hash_len" c_get_torrent_hash_len :: CUInt
foreign import ccall "libtorrent_exports.h &delete_object_with_destructor" p_delete_object_with_destructor :: FinalizerEnvPtr (CWithDestructor (Ptr a)) a

torrentPointer (TorrentSession ptr) = ptr

unpackFromMaybeDestructor :: WithDestructor (Ptr a) -> IO (Maybe (ForeignPtr a))
unpackFromMaybeDestructor ptr = if ptr == nullPtr
                                   then return Nothing
                                   else Just <$> unpackFromDestructor ptr

unpackFromDestructor :: WithDestructor (Ptr a) -> IO (ForeignPtr a)
unpackFromDestructor ptr = do
  CWithDestructor object <- peek ptr
  newForeignPtrEnv p_delete_object_with_destructor ptr object

withTorrent :: TorrentHandle -> (CTorrentHandle -> IO a) -> IO a
withTorrent handle action = withCAStringLen handle (\(str, _) -> action str)

peekTorrent' str = peekCAStringLen (str, fromIntegral c_get_torrent_hash_len)
peekTorrent = flip withForeignPtr peekTorrent'

instance Storable stored => Storable (CWithDestructor stored) where
  alignment _ = #{alignment h_with_destructor}
  sizeOf _ = #{size h_with_destructor}
  peek ptr = do
    object <- #{peek h_with_destructor, object} ptr
    return (CWithDestructor object)
  poke ptr (CWithDestructor object) = do
    #{poke h_with_destructor, object} ptr object

instance Storable (TorrentFile) where
  alignment _ = #{alignment torrent_file_info}
  sizeOf _ = #{size torrent_file_info}
  poke ptr _ = return ()
  peek ptr = do
    filename <- #{peek torrent_file_info, filename} ptr
    filename' <- peekCString filename
    startPiece <- #{peek torrent_file_info, start_piece} ptr :: IO (CUInt)
    startPieceOffset <- #{peek torrent_file_info, start_piece_offset} ptr :: IO (CUInt)
    filesize <- #{peek torrent_file_info, filesize} ptr :: IO (CUInt)
    return $ TorrentFile { _filename = filename'
                         , _filesize = fromIntegral filesize
                         , _pieceStart = fromIntegral startPiece
                         , _pieceStartOffset = fromIntegral startPieceOffset }

instance Storable (TorrentInfo) where
  alignment _ = #{alignment torrent_files_info}
  sizeOf _ = #{size torrent_files_info}
  poke ptr _ = return ()
  peek ptr = do
    num_files <- #{peek torrent_files_info, num_files} ptr :: IO (CUInt)
    filesPath <- #{peek torrent_files_info, save_path} ptr
    filesPath' <- peekCString filesPath
    files <- #{peek torrent_files_info, files} ptr
    files' <- mapM (peekElemOff files) $ take (fromIntegral num_files) [0..]
    pieceSize <- #{peek torrent_files_info, piece_size} ptr :: IO (CUInt)
    return (TorrentInfo { _torrentFiles = files', _pieceSize = fromIntegral pieceSize, _filesPath = filesPath' })

instance Storable (TorrentAlert) where
  alignment _ = #{alignment alert_type}
  sizeOf _ = #{size alert_type}
  poke ptr _ = return ()
  peek ptr = do
    alertType <- #{peek alert_type, alert_type} ptr :: IO CInt
    alertWhat' <- #{peek alert_type, alert_what} ptr
    alertWhat <- peekCString alertWhat'
    alertCategory <- #{peek alert_type, alert_category} ptr :: IO CInt
    alertTorrent' <- #{peek alert_type, torrent} ptr
    alertTorrent <- if alertTorrent' == nullPtr
                      then return Nothing
                      else Just <$> peekTorrent' alertTorrent'
    alertPiece <- #{peek alert_type, torrent_piece} ptr :: IO CUInt
    alertBuffer' <- #{peek alert_type, read_buffer} ptr
    alertBufferSize <- #{peek alert_type, read_buffer_size} ptr :: IO CUInt
    alertBuffer <- if alertBuffer' == nullPtr
                      then return Nothing
                      else Just <$> B.packCStringLen (alertBuffer', fromIntegral alertBufferSize)
    return $ Alert { _alertType = fromIntegral alertType
                   , _alertWhat = alertWhat
                   , _alertCategory = fromIntegral alertCategory
                   , _alertTorrent = alertTorrent
                   , _alertPiece = fromIntegral alertPiece
                   , _alertBuffer = alertBuffer
                   }

instance Storable ValuelessPointer where
  alignment _ = 1
  sizeOf _ = 0
  peek _ = return ValuelessPointer
  poke _ _ = return ()

unpackArrayPtr :: Storable a => (a -> IO (Maybe b)) -> Ptr a -> IO (Maybe [b])
unpackArrayPtr unpack a = if a == nullPtr
                      then return Nothing
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
