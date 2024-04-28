{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SizePrefixedArray where
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.C.Types
import GHC.IsList

-- | A marker type, not usable directly.
-- It is only exposed as `ForeignPtr (SizePrefixedArray a)` or `Ptr (SizePrefixedArray a)`.
--
-- It indicates that the memory the pointer points to ought to have the following layout:
--
-- - A `size`, of 8 bytes (on 64-bit systems) (or 4 bytes on 32-bit systems), of the type `CSize`.
-- - Followed by `size` elements of `a`, each `sizeOf @a` elements (as defined by the `Storable a` instance).
--
-- Or, in pseudo-C:
-- ```C
-- struct {
--   size_t size;      // A single word size
--   a elements[size]; // Exactly `size` elements
-- }
-- ```
data SizePrefixedArray a

-- | Writes (copies) the `list` to a `SizePrefixedArray`
--
-- The `ForeignPtr` will be cleaned up only if it is not referenced anymore.
--
-- It is guaranteed that the memory is allocated internally using `Foreign.Marshal.Alloc.malloc`
--
-- So it is okay to use `Foreign.Marshal.Alloc.realloc` from it from external (FFI) code.
-- (as long as you make sure that the external code uses the same `malloc`/`realloc`/`free`.
-- the easiest way to guarantee this is to expose the actual functions of `Foreign.Marshal.Alloc` in your FFI exports as well.)
toPointer :: forall list. (IsList list, Storable (Item list)) => list -> IO (ForeignPtr (SizePrefixedArray (Item list)))
toPointer elems = do
  rawPtr <- toRawPointer elems
  newForeignPtr finalizerFree rawPtr

-- | Extracts (copies) the elems of a `SizePrefixedArray` to `list`
--
-- The `ForeignPtr` will be cleaned up only if it is not referenced anymore.
fromPointer :: forall list. (IsList list, Storable (Item list)) => ForeignPtr (SizePrefixedArray (Item list)) -> IO list
fromPointer ptr = withForeignPtr ptr $ \rawPtr -> fromRawPointerNofree rawPtr

-- | Writes the `IsList` to a `SizePrefixedArray`
--
-- The memory is allocated using `Foreign.Marshal.Alloc.malloc`
-- which means that 
-- - it can be passed to `Foreign.Marshal.Alloc.realloc`;
-- - it should be passed to `Foreign.Marshal.Alloc.free` when done.
--
-- Prefer usage of `toPointer` unless you're
-- - _immediately_ passing the resulting pointer to a foreign function
-- and you have the expectation that the foreign code will clean it up.
-- - _returning_ this pointer from a Haskell function across a foreign boundary
-- (in this case, there's no way to reliably keep a `ForeignPtr` alive.)
toRawPointer :: forall list. (IsList list, Storable (Item list)) => list -> IO (Ptr (SizePrefixedArray (Item list)))
toRawPointer elems = do
  let list = toList elems
  let size = length list 
  ptr <- mallocBytes (wordsize + size * sizeOf' @(Item list))
  poke (sizePtr ptr) (fromIntegral size)
  pokeArray (elemsPtr ptr) list
  pure ptr

-- | Copies `list` out from inside the `SizePrefixedArray`.
--
-- Frees the `Ptr` before returning.
fromRawPointer :: forall list. (IsList list, Storable (Item list)) => Ptr (SizePrefixedArray (Item list)) -> IO list
fromRawPointer ptr = do
  elems <- fromRawPointerNofree ptr
  free ptr
  pure elems

-- | Copies `list` out from inside the `SizePrefixedArray`.
--
-- Does _not_ free the `Ptr`. Be sure to call `Foreign.Marshal.Alloc.free` yourself later.
fromRawPointerNofree :: forall list. (IsList list, Storable (Item list)) => Ptr (SizePrefixedArray (Item list)) -> IO list
fromRawPointerNofree ptr = do
  size <- fromIntegral <$> peek (sizePtr ptr)
  elems <- peekArray size (elemsPtr ptr)
  pure (fromListN size elems)

sizePtr :: Ptr (SizePrefixedArray a) -> Ptr CSize
sizePtr = castPtr

elemsPtr :: Ptr (SizePrefixedArray a) -> Ptr a
elemsPtr ptr = castPtr (ptr `plusPtr` wordsize)

wordsize :: Int
wordsize = sizeOf @Word undefined

sizeOf' :: forall a. Storable a => Int
sizeOf' = sizeOf (undefined :: a)
