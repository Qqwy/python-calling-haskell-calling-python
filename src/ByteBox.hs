{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{- | A wrapper of the common idiom 
of passing a bytestring together with its length 
back-and-forth over a foreign function interface boundary.

This module is relatively low-level. This means:
- To ensure easier reasoning about sequencing, nearly all functions use IO
- Almost all functions come with safety invariants that the caller should uphold; these are not specifically prefixed with `unsafe`.
-}
module ByteBox where
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr
import Foreign.C
import qualified Foreign.Marshal
import Foreign.Storable
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe

newtype ByteBox = ByteBox (Ptr CStringLen)

instance Show ByteBox where
  show (ByteBox ptr) = unsafePerformIO $ do 
    bytestring <- toBorrowingByteString (ByteBox ptr)
    pure ("ByteBox " <> show ptr <> " " <> show (ByteString.length bytestring) <> " " <> show bytestring)

inferno :: (ByteBox -> ByteBox -> IO ()) -> ByteString -> IO ByteString
inferno rawFun = \input -> 
  withByteStringAsByteBox input $ \inputBox ->
    alloca $ \outputBox -> do
      () <- rawFun inputBox outputBox
      toOwningByteString outputBox

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
toBorrowingByteString :: ByteBox -> IO ByteString
toBorrowingByteString bb = do
  cstringlen <- peekIntoCStringLen bb
  ByteString.Unsafe.unsafePackCStringLen cstringlen

withBorrowingByteString :: ByteBox -> (ByteString -> IO a) -> IO a
withBorrowingByteString bb action = toBorrowingByteString bb >>= action


-- O(1). The ByteString takes over ownership of the allocation behind the ByteBox
-- (it will be freed when the ByteString is GC'd.)
--
-- Unsafe:
-- 1. If the original buffer were to be changed after this function returns, 
-- this would show up in the ByteString
-- which would break referential transparency
-- 
-- 2. Do not free the original ByteBox as that would cause a double-free.
toOwningByteString :: ByteBox -> IO ByteString
toOwningByteString bb = do
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
-- - allocated using `alloca` (or any of the `with*` wrappers), which will clean up once exiting the scope
-- - read from a borrowed ptr that someone else is supposed to clean up.
free :: ByteBox -> IO ()
free (ByteBox ptr) = do
  Foreign.Marshal.free (castPtr ptr :: CString)

withStringAsByteBox :: String -> (ByteBox -> IO a) -> IO a
withStringAsByteBox str action = 
  withCStringLen str $ \cl -> 
    withCStringLenAsByteBox cl action

-- | Passes the ByteString to the function as a ByteBox. O(1)
--
-- NOTE: The function should not alter the ByteBox,
-- as changes would show up in the original bytestring.
withByteStringAsByteBox :: ByteString -> (ByteBox -> IO a) -> IO a
withByteStringAsByteBox bs action =
  ByteString.Unsafe.unsafeUseAsCStringLen bs $ \cl -> 
    withCStringLenAsByteBox cl action

-- | Passes the CStringLen to the function as a ByteBox. O(1)
--
-- Does _no_ cleanup of the internal string buffer once the function returns.
withCStringLenAsByteBox :: CStringLen -> (ByteBox -> IO a) -> IO a
withCStringLenAsByteBox cstringlen action =
  alloca $ \bb -> do
    pokeFromCStringLen bb cstringlen
    (action bb)

-- | Low-level conversion to write an (owned) ByteString into a (borrowed) ByteBox
-- O(n), has to copy the internal bytes.
--
-- The resulting ByteBox' string buffer should be freed with `free` when no longer in use.
pokeFromByteString :: ByteBox -> ByteString -> IO ()
pokeFromByteString outBox bs = do
  ByteString.Unsafe.unsafeUseAsCStringLen bs $ \(inPtr, len) -> do
    resPtr <- Foreign.Marshal.mallocBytes len
    Foreign.Marshal.copyBytes resPtr inPtr len
    pokeFromCStringLen outBox (resPtr, len)

-- | Low-level conversion between a ByteBox and a CStringLen
-- Both objects will point to the same underlying buffer
pokeFromCStringLen :: ByteBox -> CStringLen -> IO ()
pokeFromCStringLen (ByteBox ptr) (str, len) = do
  poke (castPtr ptr) str
  poke (castPtr ptr `plusPtr` sizeOf (undefined :: CString)) len

-- | Low-level conversion between a CStringLen and a ByteBox
-- Both objects will point to the same underlying buffer
peekIntoCStringLen :: ByteBox -> IO CStringLen
peekIntoCStringLen (ByteBox ptr) = do
  str <- peek (castPtr ptr)
  len <- peek (castPtr ptr `plusPtr` sizeOf (undefined ::CString))
  pure (str, len)

-- Runs the given action, passing it a newly-built (empty!) ByteBox.
--
-- This is a very low-level function, prefer using the `with*` style functions
-- which are high-level wrappers around this.
alloca :: (ByteBox -> IO a) -> IO a
alloca action = Foreign.Marshal.allocaBytesAligned sizeOf' alignment' $ \ptr -> action (ByteBox ptr)
  where
     sizeOf' = (sizeOf (undefined :: CString)) + sizeOf (undefined :: CSize)
     alignment' = max (alignment (undefined :: CString)) (alignment (undefined :: CSize))
