{-# LANGUAGE TypeFamilies #-}

module IntoOwned where

import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)

class IntoOwned a where
  type Owned a
  intoOwned :: a -> IO (Owned a)

class PtrIntoForeignPtr a where
  destructor :: (Ptr a) -> IO ()
  concurrentDestructor :: (Ptr a) -> Bool
  concurrentDestructor = const True
  intoForeignPtr :: Ptr a -> IO (ForeignPtr a)
  intoForeignPtr a =
    if concurrentDestructor a
      then Foreign.Concurrent.newForeignPtr a $ destructor a
      else newForeignPtr a $ destructor a

instance (PtrIntoForeignPtr a) => IntoOwned (Ptr a) where
  type Owned (Ptr a) = ForeignPtr a
  intoOwned = intoForeignPtr
