{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module FatPtr (FatPtr, Storable(..), clear) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.C.Types
import GHC.IsList (IsList, Item)
import GHC.IsList qualified as IsList

-- | Intended to only be used behind a `Ptr` or `ForeignPtr`.
--
-- The memory layout behind the pointer is:
-- - First a pointer to a consecutive sequence of items
-- - Then the size of this sequence, as `CSize`.
--
-- Or, equivalently in pseudo-C:
--
-- ```
-- struct FatPtr {
--   elem_type* elems;
--   size_t size;
-- };
-- ```
-- (where `elem_type` is whatever `Storable` ends up writing for `elem`)
--
-- The internal `elems` pointer is allocated using `Foreign.Marshal.Alloc.malloc`
--
newtype FatPtr elem = FatPtr elem

instance (IsList list, Storable item, (Item list) ~ item) => Storable (FatPtr item) where
  sizeOf _ = (sizeOf' @(Ptr (Item list))) + sizeOf' @CSize
  alignment _ = max (alignment' @(Ptr (Item list))) (alignment' @CSize)
  peek ptr = do
    size <- peek (sizePtr ptr)
    elems <- peekArray size (elemsPtr ptr)
    pure (IsList.fromListN size elems)

  -- | Make sure to call `initialize` on the `FatPtr` before `poke`ing it!
  -- 
  -- As it internally uses `Foreign.Marshal.Alloc.reallocBytes`, it is important that the internal element pointer
  -- is a valid pointer (either a `nullPtr` or a pointer to valid allocated memory) before the call to `poke`.
  -- This can be ensured by either using `calloc` or by calling `initialize`.
  poke ptr elems = do
    let size = length elems
    poke (sizePtr ptr) size
    newElemsPtr <- reallocBytes (elemsPtr ptr) (size * sizeOf' (Item listlike))
    pokeArray (newElemsPtr ptr) (IsList.toList elems)

fromList :: (IsList list) => list -> IO (ForeignPtr (FatPtr (Item list)))
fromList list = do
  ptr <- allocForeignPtr
  withForeignPtr $ \rawPtr -> do
    initialize rawPtr
    poke ptr list
    addForeignPtrConcFinalizer ptr (finalize rawPtr)
  pure ptr

toList :: (IsList list) => ForeignPtr (FatPtr (Item list)) -> IO list
toList ptr = withForeignPtr ptr peek
  

-- | Given garbage memory (such as returned by `malloc` or `alloca`),
-- initialize a valid empty `FatPtr` in this memory.
--
-- If `calloc` was used for initialization, the `FatPtr` is already initialized correctly.
initialize :: Ptr (FatPtr elem) -> IO (Ptr (FatPtr elem))
initialize ptr = do
  poke (sizePtr ptr) 0
  poke (elemsPtr ptr) nullPtr

-- | Cleans up the internal buffer of the `FatPtr`, leaving it in an invalid 'garbage' state.
--
-- Make sure that the `FatPtr` is not used afterwards (unless `initialize` is called again)
finalize ptr = do
  free (elemsPtr ptr)

-- | Empties a FatPtr, `free`ing the internal buffer and setting its size to `0`.
--
-- After this function the `FatPtr` is in a valid and known state:
-- - size is `0`
-- - the buffer pointer is a `nullPtr`
clear :: Ptr (FatPtr elem) -> IO (Ptr (FatPtr elem))
clear ptr = do
  finalize ptr
  initialize ptr

sizeOf' :: forall a. Storable a => Int
sizeOf' = sizeOf (undefined :: a)

alignment' :: forall a. Storable a => Int
alignment' = alignment (undefined :: a)

sizePtr :: forall a. Ptr (FatPtr a) -> Ptr CSize
sizePtr = castPtr (ptr `plusPtr` sizeOf' @(Ptr a))

elemsPtr :: Ptr (FatPtr a) -> Ptr a
elemsPtr ptr = castPtr (ptr `plusPtr` wordsize)
