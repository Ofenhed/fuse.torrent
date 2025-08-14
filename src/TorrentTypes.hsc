{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module TorrentTypes where

import Control.Exception (bracket)
import Foreign
import IntoOwned (PtrIntoForeignPtr(destructor, concurrentDestructor))
import qualified Foreign.Concurrent
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM)
import Data.Bits (toIntegralSized)
import Data.Data (Data(..), Typeable(..))
import Data.Either (fromRight)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Word (Word32)

import Control.Lens
import Language.C.Types.Parse (cIdentifierFromString)
import System.Posix.Types (COff)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Unsafe as B.Unsafe

#include "libtorrent_exports.h"
data ValuelessPointer = ValuelessPointer deriving Eq
newtype CWithDestructor a = CWithDestructor a
type WithDestructor a = Ptr (CWithDestructor a)

type LTInt = Int32
type LTWord = Word32
type LTCInt = CInt
type LTCUInt = CUInt

type TorrentPieceType = LTInt
type TorrentPieceOffsetType = LTInt
type TorrentPieceSizeType = LTInt

data LibTorrentAlert
data CTorrentSession
type TorrentSession = Ptr CTorrentSession

type TorrentHash = B.ByteString
data InfoHash
data CTorrentInfo
data CTorrentHandle
type TorrentHandle = ForeignPtr CTorrentHandle
data CAddTorrentParams

data TorrentFile = TorrentFile { _filename :: FilePath
                               , _pieceStart :: TorrentPieceType
                               , _pieceStartOffset :: TorrentPieceOffsetType
                               , _filesize :: COff } deriving Show
makeLenses ''TorrentFile
data TorrentInfo = TorrentInfo { _torrentFiles :: [TorrentFile]
                               , _pieceSize :: TorrentPieceSizeType
                               , _filesPath :: FilePath } deriving Show

makeLenses ''TorrentInfo
data CAlert = CAlert
type Alert = Ptr CAlert

data NewTorrentType = NewMagnetTorrent String
                    | NewTorrentFile B.ByteString

hashToHex :: TorrentHash -> String
hashToHex = LC8.unpack . BB.toLazyByteString . BB.lazyByteStringHex . LC8.fromStrict

peekSha1' str = B.packCStringLen (str, 20)
peekSha1 = flip withForeignPtr peekSha1'

C.context $ C.cppCtx <> C.cppTypePairs
  [(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::torrent_handle", [t| CTorrentHandle |]),
   (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::session", [t| CTorrentSession |])
  ]
C.include "libtorrent/torrent_handle.hpp"
C.include "libtorrent/info_hash.hpp"
C.include "libtorrent/session.hpp"

instance PtrIntoForeignPtr CTorrentHandle where
  concurrentDestructor = const False
  destructor ptr = [C.exp| void { delete $(lt::torrent_handle* ptr) } |]

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
    startPiece <- #{peek torrent_file_info, start_piece} ptr
    startPieceOffset <- #{peek torrent_file_info, start_piece_offset} ptr
    filesize <- #{peek torrent_file_info, filesize} ptr
    return $ TorrentFile { _filename = filename'
                         , _filesize = filesize
                         , _pieceStart = startPiece
                         , _pieceStartOffset = startPieceOffset }

instance Storable (TorrentInfo) where
  alignment _ = #{alignment torrent_files_info}
  sizeOf _ = #{size torrent_files_info}
  poke ptr _ = return ()
  peek ptr = do
    num_files <- #{peek torrent_files_info, num_files} ptr :: IO LTCInt
    filesPath <- #{peek torrent_files_info, save_path} ptr
    filesPath' <- peekCString filesPath
    files <- #{peek torrent_files_info, files} ptr
    files' <- mapM (peekElemOff files) $ take (fromJust $ toIntegralSized num_files) [0..]
    pieceSize <- #{peek torrent_files_info, piece_size} ptr
    return (TorrentInfo { _torrentFiles = files', _pieceSize = pieceSize, _filesPath = filesPath' })

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
