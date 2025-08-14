{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Utils where

import qualified Data.ByteString.Unsafe as B.Unsafe
import qualified Language.C.Inline as C
import qualified Foreign.C as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Foreign (FunPtr, castFunPtr, castPtr, alloca, peek, withForeignPtr, free)
import Foreign.Ptr (Ptr)
import Foreign.Concurrent (newForeignPtr)
import TorrentTypes (InfoHash, TorrentHash)
import TorrentContext (torrentContext)
import InlineTypes (BoostSharedArray, StdString, StdVector)
import qualified Data.ByteString as B
import Foreign.ForeignPtr (ForeignPtr)

import qualified Debug.Trace as Trace'
import Data.Kind (Type)
import GHC.Types (type (~~))

C.context $ torrentContext
C.include "libtorrent/session.hpp"

getBestHash :: FunPtr (Ptr a -> IO (InfoHash)) -> Ptr a -> IO TorrentHash
getBestHash target a = do
  let target' = castFunPtr target
  let a' = castPtr a
  alloca $ \size -> do
    buf <- [C.block| char* {
        auto callback = $(lt::info_hash_t(*target')(void*));
        auto info = callback($(void* a'));
        bool has_v2 = info.has_v2();
        auto size = has_v2 ? 32 : 20;
        *$(size_t *size) = size;
        char *buf = (char*)malloc(size);
        auto hash = has_v2 ? info.v2.data() : info.v1.data();
        memcpy(buf, hash, size);
        return buf;
        } |]
    size' <- peek size
    B.Unsafe.unsafePackMallocCStringLen (buf, fromIntegral size')

getBestHash' :: ForeignPtr InfoHash -> IO TorrentHash
getBestHash' = flip withForeignPtr $ getBestHash handler
  where handler = [C.funPtr| lt::info_hash_t copy_info_hash(lt::info_hash_t *h) { return *h; } |]

newInfoHashContainer :: Ptr InfoHash -> IO (ForeignPtr InfoHash)
newInfoHashContainer h = newForeignPtr h (free h)

resolveInfoHash :: Ptr InfoHash -> IO TorrentHash
resolveInfoHash h = newInfoHashContainer h >>= getBestHash'

withCStringCLen :: String -> ((Ptr C.CChar, C.CSize) -> IO a) -> IO a
withCStringCLen str f = C.withCStringLen str (\(dest, len) -> f (dest, C.CSize $ fromIntegral len))

wrapSharedArray' :: Ptr (BoostSharedArray C.CChar) -> C.CSize -> Ptr C.CChar -> IO B.ByteString
wrapSharedArray' arr size buf = B.Unsafe.unsafePackCStringFinalizer
          (castPtr buf)
          (fromIntegral size)
          [C.exp| void { delete $(boost::shared_array<char>* arr) } |]

wrapSharedArray :: Ptr (BoostSharedArray C.CChar) -> C.CSize -> IO B.ByteString
wrapSharedArray arr size = wrapSharedArray' arr size buf
  where
  buf = [CU.pure| const char* {
      $(boost::shared_array<char>* arr)->get()
  } |]

wrapVec' :: Ptr (StdVector C.CChar) -> C.CSize -> Ptr C.CChar -> IO B.ByteString
wrapVec' arr size buf = B.Unsafe.unsafePackCStringFinalizer
          (castPtr buf)
          (fromIntegral size)
          [C.exp| void { delete $(std::vector<char>* arr) } |]

wrapVec :: Ptr (StdVector C.CChar) -> C.CSize -> IO B.ByteString
wrapVec arr size = wrapVec' arr size buf
  where
  buf = [CU.pure| const char* {
      $(std::vector<char>* arr)->data()
  } |]

wrapStdString :: Ptr StdString -> IO B.ByteString
wrapStdString str = do
  alloca $ \size -> do
    cbuf <- [CU.block| const char* {
        auto s = $(std::string* str);
        *$(size_t* size) = s->size();
        return s->c_str();
    } |]
    size' <- peek size
    B.Unsafe.unsafePackCStringFinalizer
          (castPtr cbuf)
          (fromIntegral size')
          [C.exp| void { delete $(std::string* str) } |]

data OptionalTrace = Trace | NoTrace

withTrace :: Bool -> OptionalTrace
withTrace True = Trace
withTrace False = NoTrace

class (WithDebug t ~ d, OptionalDebug d d) => OptionalDebug t d where
  type WithDebug t :: Type
  traceObject :: t -> d
  trace :: t -> String -> a -> a
  trace = trace . traceObject
  traceShow :: Show a => t -> a -> b -> b
  traceShow = traceShow . traceObject
  traceShowM :: (Applicative m, Show a) => t -> a -> m ()
  traceShowM = traceShowM . traceObject
  traceM :: Applicative m => t -> String -> m ()
  traceM = traceM . traceObject
  traceShowId :: Show a => t -> a -> a
  traceShowId = traceShowId . traceObject

instance OptionalDebug OptionalTrace OptionalTrace where
  type WithDebug OptionalTrace = OptionalTrace
  traceObject = id
  trace NoTrace = \_ -> id
  trace Trace = Trace'.trace
  traceShow NoTrace = \_ -> id
  traceShow Trace = Trace'.traceShow
  traceShowM NoTrace = \_ -> pure ()
  traceShowM Trace = Trace'.traceShowM
  traceM NoTrace = \_ -> pure ()
  traceM Trace = Trace'.traceM
  traceShowId NoTrace = id
  traceShowId Trace = Trace'.traceShowId

-- | Alias for quot
(/.) :: Integral a => a -> a -> a
(/.) = quot

-- | Get product and modulus value
(/%) :: Integral b => b -> b -> (b, b)
x /% y = let q = x /. y
             m = mod x y
            in (q, m)

-- | Quot with ceiling
(/^) :: Integral a => a -> a -> a
x /^ y = let (q, m) = x /% y
                 in q + if m /= 0 then 1 else 0
