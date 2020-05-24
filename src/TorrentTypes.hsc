{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

import Debug.Trace

#include "libtorrent_exports.h"
data ValuelessPointer = ValuelessPointer deriving Eq
newtype CWithDestructor a = CWithDestructor a
type WithDestructor a = Ptr (CWithDestructor a)

type CTorrentSession = Ptr ValuelessPointer
data TorrentSession = TorrentSession CTorrentSession
type CTorrentHandle = ValuelessPointer
type TorrentHandle = ForeignPtr CTorrentHandle
data TorrentFile = TorrentFile String Word deriving Show
data TorrentInfo = TorrentInfo [TorrentFile] deriving Show
data CAlert = CAlert
type Alert = Ptr CAlert
data TorrentAlert = Alert { alertType :: Int, alertWhat :: String } deriving (Show)

foreign import ccall "libtorrent_exports.h &delete_object_with_destructor" p_delete_object_with_destructor :: FinalizerEnvPtr (CWithDestructor (Ptr a)) a

torrentPointer (TorrentSession ptr) = ptr

unpackFromDestructor :: WithDestructor (Ptr a) -> IO (ForeignPtr a)
unpackFromDestructor ptr = do
  CWithDestructor object <- peek ptr
  newForeignPtrEnv p_delete_object_with_destructor ptr object

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
  peek ptr = do
    filename <- #{peek torrent_file_info, filename} ptr
    filename' <- peekCString filename
    filesize <- #{peek torrent_file_info, filesize} ptr :: IO (CUInt)
    return (TorrentFile filename' $ fromIntegral filesize)
  poke ptr _ = return ()

instance Storable (TorrentInfo) where
  alignment _ = #{alignment torrent_files_info}
  sizeOf _ = #{size torrent_files_info}
  peek ptr = do
    num_files <- #{peek torrent_files_info, num_files} ptr :: IO (CUInt)
    files <- #{peek torrent_files_info, files} ptr
    files' <- mapM (peekElemOff files) $ take (fromIntegral num_files) [0..]
    return (TorrentInfo files')
  poke ptr _ = return ()

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
