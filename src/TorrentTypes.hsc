{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module TorrentTypes where

import Control.Exception (bracket)
import Foreign
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
data TorrentAlert = Alert { _alertType :: LTInt
                          , _alertWhat :: String
                          , _alertError :: Maybe String
                          , _alertCategory :: LTInt
                          , _alertTorrent :: Maybe TorrentHandle
                          , _alertPiece :: TorrentPieceType
                          , _alertInfoHashes :: Maybe (ForeignPtr InfoHash)
                          , _alertBuffer :: Maybe B.ByteString } deriving (Show)
makeLenses ''TorrentAlert

data NewTorrentType = NewMagnetTorrent String
                    | NewTorrentFile B.ByteString

hashToHex :: TorrentHash -> String
hashToHex = LC8.unpack . BB.toLazyByteString . BB.lazyByteStringHex . LC8.fromStrict

peekSha1' str = B.packCStringLen (str, 20)
peekSha1 = flip withForeignPtr peekSha1'

C.context $ C.cppCtx <> C.cppTypePairs
  [(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::torrent_handle", [t| CTorrentHandle |])
  ]
C.include "libtorrent/torrent_handle.hpp"
C.include "libtorrent/info_hash.hpp"

wrapTorrentHandle :: Ptr CTorrentHandle -> IO TorrentHandle
wrapTorrentHandle handle = do
  let dealloc = [C.funPtr| void deleteHandle(lt::torrent_handle *ptr) { delete ptr; } |]
  newForeignPtr dealloc handle

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

instance Storable (TorrentAlert) where
  alignment _ = #{alignment alert_type}
  sizeOf _ = #{size alert_type}
  poke ptr _ = return ()
  peek ptr = do
    alertType <- #{peek alert_type, alert_type} ptr
    alertWhat' <- #{peek alert_type, alert_what} ptr
    alertWhat <- peekCString alertWhat'
    alertError' <- #{peek alert_type, error_message} ptr
    alertError <- if alertError' == nullPtr
                     then return Nothing
                     else Just <$> peekCString alertError'
    alertCategory <- #{peek alert_type, alert_category} ptr
    alertTorrent' <- #{peek alert_type, torrent} ptr
    alertTorrent <- if alertTorrent' == nullPtr
                      then return Nothing
                      else Just <$> wrapTorrentHandle alertTorrent'
    alertPiece <- #{peek alert_type, torrent_piece} ptr
    alertBuffer' <- #{peek alert_type, read_buffer} ptr
    alertBufferSize <- #{peek alert_type, read_buffer_size} ptr :: IO LTCInt
    alertBuffer <- if alertBuffer' == nullPtr
                      then return Nothing
                      else do buf <- [CU.exp| const char* { static_cast<boost::shared_array<char>*>($(void *alertBuffer'))->get() } |]
                              Just <$> B.Unsafe.unsafePackCStringFinalizer
                                         (castPtr buf)
                                         (fromJust $ toIntegralSized alertBufferSize)
                                         [CU.exp| void { delete static_cast<boost::shared_array<char>*>($(void *alertBuffer')) } |]
    alertInfoHashes' <- #{peek alert_type, info_hashes} ptr
    alertInfoHashes <- if alertInfoHashes' == nullPtr
                          then return Nothing
                          else (newForeignPtr_ (castPtr alertInfoHashes') >>= \ptr -> addForeignPtrFinalizer finalizerFree ptr >> return (Just ptr))
    return $ Alert { _alertType = alertType
                   , _alertWhat = alertWhat
                   , _alertError = alertError
                   , _alertCategory = alertCategory
                   , _alertTorrent = alertTorrent
                   , _alertPiece = alertPiece
                   , _alertBuffer = alertBuffer
                   , _alertInfoHashes = alertInfoHashes
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
