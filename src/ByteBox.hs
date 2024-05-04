{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module ByteBox where
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr
import Foreign.C
import qualified Foreign.Marshal
import Foreign.Storable
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe

newtype ByteBox = ByteBox (Ptr ByteBox)

instance Show ByteBox where
  show bb = unsafePerformIO $ do 
    bytestring <- unsafeToBorrowingByteString bb
    pure (show bytestring)

-- O(1). The resulting ByteString shares the underlying buffer.
--
-- The ByteString 'borrows' the buffer, it has no finalizer registered,
-- so:
-- - the caller has to clean up the ByteBox
-- - but ensure that the last ByteString referencing it has gone out of scope beforehand!
--
-- Unsafe: If the original buffer were to be changed after this function returns, 
-- this would show up in the ByteString
-- which would break referential transparency
unsafeToBorrowingByteString :: ByteBox -> IO ByteString
unsafeToBorrowingByteString bb = do
  cstringlen <- peekIntoCStringLen bb
  ByteString.Unsafe.unsafePackCStringLen cstringlen

-- O(1). The ByteString takes over ownership of the allocation behind the ByteBox
-- (it will be freed when the ByteString is GC'd.)
--
-- Unsafe:
-- 1. If the original buffer were to be changed after this function returns, 
-- this would show up in the ByteString
-- which would break referential transparency
-- 
-- 2. Do not free the original ByteBox as that would cause a double-free.
unsafeToOwningByteString :: ByteBox -> IO ByteString
unsafeToOwningByteString bb = do
  cstringlen <- peekIntoCStringLen bb
  ByteString.Unsafe.unsafePackMallocCStringLen cstringlen

-- O(n), does a `memcpy` to copy the contents of the ByteBox into a new owned ByteString.
copyToByteString :: ByteBox -> IO ByteString
copyToByteString bb = do
  cstringlen <- peekIntoCStringLen bb
  ByteString.packCStringLen cstringlen

-- Frees the internal string buffer.
--
-- After calling this function, the contents of the ByteBox should not be used any more
--
-- Note that the ByteBox' pointer itself is _not_ cleaned up;
-- it is expected that the ByteBox itself is either:
-- - allocated using `alloca`, which will clean up once exiting the scope
-- - read from a borrowed ptr that someone else is supposed to clean up.
unsafeFree :: ByteBox -> IO ()
unsafeFree (ByteBox ptr) = do
  Foreign.Marshal.free (castPtr ptr :: CString)

withByteStringAsByteBox :: ByteString -> (ByteBox -> IO a) -> IO a
withByteStringAsByteBox bs action =
  ByteString.useAsCStringLen bs $ \cstringlen -> 
    withCStringLenAsByteBox cstringlen action

withCStringLenAsByteBox :: CStringLen -> (ByteBox -> IO a) -> IO a
withCStringLenAsByteBox cstringlen action =
  alloca $ \bb -> do
    pokeFromCStringLen bb cstringlen
    action bb

pokeFromCStringLen :: ByteBox -> CStringLen -> IO ()
pokeFromCStringLen (ByteBox ptr) (str, len) = do
  poke (castPtr ptr) str
  poke (castPtr ptr `plusPtr` sizeOf (undefined :: CString)) len

peekIntoCStringLen :: ByteBox -> IO CStringLen
peekIntoCStringLen (ByteBox ptr) = do
  str <- peek (castPtr ptr)
  len <- peek (castPtr ptr `plusPtr` sizeOf (undefined ::CString))
  pure (str, len)

alloca :: (ByteBox -> IO a) -> IO a
alloca action = Foreign.Marshal.allocaBytesAligned sizeOf' alignment' $ \ptr -> action (ByteBox ptr)
  where
     sizeOf' = (sizeOf (undefined :: CString)) + sizeOf (undefined :: CSize)
     alignment' = max (alignment (undefined :: CString)) (alignment (undefined :: CSize))
