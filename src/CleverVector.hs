{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module CleverVector where
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.Types
import GHC.IsList

data SizePrefixedArray a

toMallocPointer :: forall list. (IsList list, Storable (Item list)) => list -> IO (Ptr (SizePrefixedArray (Item list)))
toMallocPointer elems = do
    let list = toList elems
    let size = length list 
    ptr <- mallocBytes (wordsize + size * sizeOf' @(Item list))
    poke (sizePtr ptr) (fromIntegral size)
    pokeArray (elemsPtr ptr) list
    pure ptr

fromPointerNofree :: (IsList list, Storable (Item list)) => Ptr (SizePrefixedArray (Item list)) -> IO list
fromPointerNofree ptr = do
    size <- fmap fromIntegral peek (sizePtr ptr)
    elems <- peekArray size (elemsPtr ptr)
    pure (fromListN size elems)

fromPointer :: (IsList list, Storable (Item list)) => Ptr (SizePrefixedArray (Item list)) -> IO list
fromPointer ptr = do
    elems <- fromPointerNofree ptr
    free ptr
    pure elems

sizePtr :: Ptr (SizePrefixedArray a) -> Ptr Word
sizePtr = castPtr

elemsPtr :: Ptr (SizePrefixedArray a) -> Ptr a
elemsPtr ptr = castPtr (ptr `plusPtr` wordsize)

wordsize :: Int
wordsize = sizeOf @Word undefined

sizeOf' :: forall a. Storable a => Int
sizeOf' = sizeOf (undefined :: a)
