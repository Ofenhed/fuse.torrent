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
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Data.Bits (toIntegralSized, Bits)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B.Unsafe
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Trace'
import Foreign (castPtr)
import qualified Foreign.C as C
import Foreign.Ptr (Ptr)
import InlineTypes (BoostSharedArray, StdMutex, StdString, StdVector)
import IntoOwned
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import TorrentContext (torrentContext)
import GHC.Stack (callStack, prettyCallStack, HasCallStack)

C.context $ torrentContext
C.include "libtorrent/session.hpp"

unwrapFi :: (Integral a, Integral b, Bits a, Bits b, HasCallStack) => a -> b
unwrapFi x
  | Just b <- toIntegralSized x = b
  | otherwise = error $ "unwrapFi failed: " ++ prettyCallStack callStack

withCStringCLen :: String -> ((Ptr C.CChar, C.CSize) -> IO a) -> IO a
withCStringCLen str f = C.withCStringLen str (\(dest, len) -> f (dest, C.CSize $ unwrapFi len))

instance PtrIntoForeignPtr StdMutex where
  destructor ptr = [CU.exp| void { delete $(std::mutex* ptr) } |]

data IntoByteString where
  FromBoostSharedArray :: {intoByteStringFromBoostArray :: Ptr (BoostSharedArray C.CChar), intoByteStringLength :: C.CSize, intoByteStringRaw :: Maybe (Ptr C.CChar)} -> IntoByteString
  FromStdVec :: {intoByteStringFromStdVec :: Ptr (StdVector C.CChar), intoByteStringLength' :: Maybe C.CSize, intoByteStringRaw :: Maybe (Ptr C.CChar)} -> IntoByteString
  FromStdString :: {intoByteStringFromStdString :: Ptr StdString, intoByteStringLength' :: Maybe C.CSize, intoByteStringRaw :: Maybe (Ptr C.CChar)} -> IntoByteString

instance IntoOwned IntoByteString where
  type Owned IntoByteString = B.ByteString
  intoOwned FromBoostSharedArray {intoByteStringFromBoostArray = ptr, intoByteStringLength = size, intoByteStringRaw = raw} = do
    let raw' =
          flip fromMaybe raw $
            [CU.pure| const char* {
        $(boost::shared_array<char>* ptr)->get()
    } |]
    B.Unsafe.unsafePackCStringFinalizer
      (castPtr raw')
      (unwrapFi size)
      [C.exp| void { delete $(boost::shared_array<char>* ptr) } |]
  intoOwned FromStdVec {intoByteStringFromStdVec = ptr, intoByteStringLength' = size, intoByteStringRaw = raw} = do
    let raw' =
          flip fromMaybe raw $
            [CU.pure| const char* {
      $(std::vector<char>* ptr)->data()
    } |]
        size' =
          flip fromMaybe size $
            [CU.pure| size_t {
      $(std::vector<char>* ptr)->size()
    } |]
    B.Unsafe.unsafePackCStringFinalizer
      (castPtr raw')
      (unwrapFi size')
      [C.exp| void { delete $(std::vector<char>* ptr) } |]
  intoOwned FromStdString {intoByteStringFromStdString = ptr, intoByteStringLength' = size, intoByteStringRaw = raw} = do
    let raw' =
          flip fromMaybe raw $
            [CU.pure| const char* {
      $(std::string* ptr)->data()
    } |]
        size' =
          flip fromMaybe size $
            [CU.pure| size_t {
      $(std::string* ptr)->size()
    } |]
    B.Unsafe.unsafePackCStringFinalizer
      (castPtr raw')
      (unwrapFi size')
      [C.exp| void { delete $(std::string* ptr) } |]

data OptionalTrace = Trace | NoTrace

withTrace :: Bool -> OptionalTrace
withTrace True = Trace
withTrace False = NoTrace

-- Merge two already sorted lists into a sorted list
sortedUnion :: (Ord a) => [a] -> [a] -> [a]
sortedUnion = sortedUnionBy id

sortedUnionBy :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
sortedUnionBy f a b
  | a1 : a2 : _ <- a,
    f a1 > f a2 =
      error "first list to sortedUnionBy not sorted"
  | b1 : b2 : _ <- b,
    f b1 > f b2 =
      error "first list to sortedUnionBy not sorted"
  | a1 : ax <- a,
    b1 : _ <- b,
    f a1 <= f b1 =
      a1 : sortedUnionBy f ax b
  | a1 : _ <- a,
    b1 : bx <- b,
    f b1 < f a1 =
      b1 : sortedUnionBy f a bx
  | [] <- a = b
  | [] <- b = a

class (WithDebug t ~ d, OptionalDebug d d) => OptionalDebug t d where
  type WithDebug t :: Type
  traceObject :: t -> d
  trace :: t -> String -> a -> a
  trace = trace . traceObject
  traceShow :: (Show a) => t -> a -> b -> b
  traceShow = traceShow . traceObject
  traceShowM :: (Applicative m, Show a) => t -> a -> m ()
  traceShowM = traceShowM . traceObject
  traceM :: (Applicative m) => t -> String -> m ()
  traceM = traceM . traceObject
  traceShowId :: (Show a) => t -> a -> a
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

newtype AlwaysEq a = AlwaysEq a

instance (Show a) => Show (AlwaysEq a) where
  showsPrec d (AlwaysEq a) = showsPrec d a

instance (Read a) => Read (AlwaysEq a) where
  readsPrec d = fmap (\(x, r) -> (AlwaysEq x, r)) . readsPrec d

instance Eq (AlwaysEq a) where
  _ == _ = True

-- | Alias for quot
(/.) :: (Integral a) => a -> a -> a
(/.) = quot

-- | Get product and modulus value
(/%) :: (Integral b) => b -> b -> (b, b)
x /% y =
  let q = x /. y
      m = mod x y
   in (q, m)

-- | Quot with ceiling
(/^) :: (Integral a) => a -> a -> a
x /^ y =
  let (q, m) = x /% y
   in q + if m /= 0 then 1 else 0
