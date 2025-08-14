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

import qualified Data.ByteString.Unsafe as B.Unsafe
import Foreign (FunPtr, alloca, castFunPtr, castPtr, free, peek, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline as C
import TorrentContext (torrentContext)
import TorrentTypes (InfoHash, TorrentHash)

C.context $ torrentContext
C.include "libtorrent/session.hpp"

getBestHash :: FunPtr (Ptr a -> IO (InfoHash)) -> Ptr a -> IO TorrentHash
getBestHash target a = do
  let target' = castFunPtr target
  let a' = castPtr a
  alloca $ \size -> do
    buf <-
      [C.block| char* {
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
  where
    handler = [C.funPtr| lt::info_hash_t copy_info_hash(lt::info_hash_t *h) { return *h; } |]

newInfoHashContainer :: Ptr InfoHash -> IO (ForeignPtr InfoHash)
newInfoHashContainer h = newForeignPtr h (free h)

resolveInfoHash :: Ptr InfoHash -> IO TorrentHash
resolveInfoHash h = newInfoHashContainer h >>= getBestHash'
