{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module TorrentUtils where

import Control.Monad ((<=<))
import qualified Data.ByteString.Unsafe as B.Unsafe
import Data.Maybe (fromJust)
import Foreign (FunPtr, alloca, castFunPtr, castPtr, free, peek, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import IntoOwned (IntoOwned (intoOwned), PtrIntoForeignPtr (destructor))
import qualified Language.C.Inline as C
import TorrentContext (torrentContext)
import TorrentTypes (InfoHash, TorrentHash)

C.context $ torrentContext
C.include "libtorrent/session.hpp"

getBestHash :: FunPtr (Ptr a -> Ptr InfoHash -> IO C.CBool) -> Ptr a -> IO (Maybe TorrentHash)
getBestHash target a = do
  let target' = castFunPtr target
  let a' = castPtr a
  alloca $ \size -> do
    buf <-
      [C.block| const char* {
        auto callback = $(bool(*target')(void*, lt::info_hash_t*));
        lt::info_hash_t info;
        if (callback($(void* a'), &info)) {
            bool has_v2 = info.has_v2();
            auto size = has_v2 ? 32 : 20;
            *$(size_t *size) = size;
            char *buf = (char*)malloc(size);
            auto hash = has_v2 ? info.v2.data() : info.v1.data();
            memcpy(buf, hash, size);
            return buf;
        } else {
            return NULL;
        }
    } |]
    if buf == nullPtr
      then return Nothing
      else do
        size' <- peek size
        Just <$> B.Unsafe.unsafePackMallocCStringLen (buf, fromIntegral size')

getBestHash' :: ForeignPtr InfoHash -> IO TorrentHash
getBestHash' = flip withForeignPtr $ return . fromJust <=< getBestHash handler
  where
    handler = [C.funPtr| bool copy_info_hash(lt::info_hash_t *h, lt::info_hash_t *into) { *into = *h; return true; } |]

instance PtrIntoForeignPtr InfoHash where
  destructor = free

resolveInfoHash :: Ptr InfoHash -> IO TorrentHash
resolveInfoHash = getBestHash' <=< intoOwned
