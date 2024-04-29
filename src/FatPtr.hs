{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module FatPtr where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.C.Types
import GHC.IsList (IsList, Item)
import GHC.IsList qualified as IsList
import System.IO.Unsafe (unsafePerformIO)


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
data FatPtr a = FatPtr (ForeignPtr a) CSize
  deriving (Show)

data RawFatPtr a = RawFatPtr (Ptr a) CSize
  deriving (Show)

new :: (FatPtr a)
new = unsafePerformIO $ unsafeFromRaw newRaw

newRaw :: (RawFatPtr a)
newRaw = RawFatPtr nullPtr 0

-- | 
-- Make sure the original pointer is not manually freed (or kept around in general) after this call!
unsafeFromRaw :: RawFatPtr a -> IO (FatPtr a)
unsafeFromRaw (RawFatPtr elems size) = do
  elemsPtr <- newForeignPtr finalizerFree elems
  pure (FatPtr elemsPtr size)

withRaw :: FatPtr a -> (RawFatPtr a -> IO b) -> IO b
withRaw (FatPtr elems size) action = do
  withForeignPtr elems $ \rawElemsPtr ->
    action (RawFatPtr rawElemsPtr size)

copyToRaw :: forall a. (Storable a) => FatPtr a -> IO (RawFatPtr a)
copyToRaw ptr =
  withRaw ptr $ \(RawFatPtr elemsSrc size) -> do
    elemsDst <- mallocArray (fromIntegral size)
    copyBytes elemsDst elemsSrc (sizeOf' @a * (fromIntegral size))
    pure (RawFatPtr elemsDst size)

fromList :: (IsList list, Storable (Item list)) => list -> FatPtr (Item list)
fromList list = unsafePerformIO $ do
  rawPtr <- rawFromList list
  unsafeFromRaw rawPtr

rawFromList :: (IsList list, Storable (Item list)) => list -> IO (RawFatPtr (Item list))
rawFromList list = do
  let elems = (IsList.toList list)
  let size = length elems
  elemsPtr <- mallocArray size
  pokeArray elemsPtr elems
  pure (RawFatPtr elemsPtr (fromIntegral size))

rawToListNonfree :: (IsList list, Storable (Item list)) => RawFatPtr (Item list) -> IO list
rawToListNonfree (RawFatPtr ptr size) = do
  let size' = fromIntegral size
  list <- peekArray size' ptr
  pure (IsList.fromListN size' list)

-- | Caller must make sure the original pointer is no longer used after passed to this function.
unsafeRawFree :: RawFatPtr a -> IO ()
unsafeRawFree (RawFatPtr ptr size) = do
  free ptr

-- | Caller must make sure the original pointer is no longer used after passed to this function.
unsafeRawToList :: (IsList list, Storable (Item list)) => RawFatPtr (Item list) -> IO list
unsafeRawToList ptr = do
  list <- rawToListNonfree ptr
  unsafeRawFree ptr
  pure list

toList :: (IsList list, Storable (Item list)) => FatPtr (Item list) -> list
toList ptr = unsafePerformIO $
  withRaw ptr $ \rawPtr -> do
    rawToListNonfree rawPtr

instance (Storable a) => IsList (FatPtr a) where
  type Item (FatPtr a) = a
  fromList = fromList
  toList = toList

instance Storable (RawFatPtr a) where
  sizeOf _ = (sizeOf' @(Ptr a)) + sizeOf' @CSize
  alignment _ = max (alignment' @(Ptr a)) (alignment' @CSize)
  peek ptr = do
    elems <- peek (castPtr ptr)
    size <- peek (sizePtr ptr)
    pure (RawFatPtr elems size)
  poke ptr (RawFatPtr elems size) = do
    poke (castPtr ptr) elems
    poke (sizePtr ptr) size

instance Storable (FatPtr a) where
  sizeOf _ = (sizeOf' @(Ptr a)) + sizeOf' @CSize
  alignment _ = max (alignment' @(Ptr a)) (alignment' @CSize)
  peek ptr = peek (castPtr ptr) >>= unsafeFromRaw
  poke ptr fatPtr = withRaw fatPtr (poke (castPtr ptr))


-- instance (IsList list, Storable item, (Item list) ~ item) => Storable (FatPtr item) where
--   sizeOf _ = (sizeOf' @(Ptr (Item list))) + sizeOf' @CSize
--   alignment _ = max (alignment' @(Ptr (Item list))) (alignment' @CSize)
--   peek ptr = do
--     size <- peek (sizePtr ptr)
--     elems <- peekArray size (elemsPtr ptr)
--     pure (IsList.fromListN size elems)

--   -- | Make sure to call `initialize` on the `FatPtr` before `poke`ing it!
--   -- 
--   -- As it internally uses `Foreign.Marshal.Alloc.reallocBytes`, it is important that the internal element pointer
--   -- is a valid pointer (either a `nullPtr` or a pointer to valid allocated memory) before the call to `poke`.
--   -- This can be ensured by either using `calloc` or by calling `initialize`.
--   poke ptr elems = do
--     let size = length elems
--     poke (sizePtr ptr) size
--     newElemsPtr <- reallocBytes (elemsPtr ptr) (size * sizeOf' (Item listlike))
--     pokeArray (newElemsPtr ptr) (IsList.toList elems)

-- fromList :: (IsList list) => list -> IO (ForeignPtr (FatPtr (Item list)))
-- fromList list = do
--   ptr <- allocForeignPtr
--   withForeignPtr $ \rawPtr -> do
--     initialize rawPtr
--     poke ptr list
--     addForeignPtrConcFinalizer ptr (finalize rawPtr)
--   pure ptr

-- toList :: (IsList list) => ForeignPtr (FatPtr (Item list)) -> IO list
-- toList ptr = withForeignPtr ptr peek
  

-- | Given garbage memory (such as returned by `malloc` or `alloca`),
-- initialize a valid empty `FatPtr` in this memory.
--
-- If `calloc` was used for initialization, the `FatPtr` is already initialized correctly.
-- initialize :: Ptr (FatPtr elem) -> IO (Ptr (FatPtr elem))
-- initialize ptr = do
--   poke (sizePtr ptr) 0
--   poke (elemsPtr ptr) nullPtr

-- | Cleans up the internal buffer of the `FatPtr`, leaving it in an invalid 'garbage' state.
--
-- Make sure that the `FatPtr` is not used afterwards (unless `initialize` is called again)
-- finalize ptr = do
--   free (elemsPtr ptr)

-- -- | Empties a FatPtr, `free`ing the internal buffer and setting its size to `0`.
-- --
-- -- After this function the `FatPtr` is in a valid and known state:
-- -- - size is `0`
-- -- - the buffer pointer is a `nullPtr`
-- clear :: Ptr (FatPtr elem) -> IO (Ptr (FatPtr elem))
-- clear ptr = do
--   finalize ptr
--   initialize ptr

sizeOf' :: forall a. Storable a => Int
sizeOf' = sizeOf (undefined :: a)

alignment' :: forall a. Storable a => Int
alignment' = alignment (undefined :: a)

sizePtr :: forall a. Ptr (RawFatPtr a) -> Ptr CSize
sizePtr ptr = castPtr (ptr `plusPtr` sizeOf' @(Ptr a))

elemsPtr :: Ptr (RawFatPtr a) -> Ptr a
elemsPtr ptr = castPtr ptr
