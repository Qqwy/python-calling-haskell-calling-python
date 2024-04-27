{-# LANGUAGE ForeignFunctionInterface #-}
module Ouroboros where
import qualified Data.Char as C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Control.Concurrent.Async (async, wait)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SVector

foreign export ccall example :: CString -> IO CString
example :: CString -> IO CString 
example cstr = do
  charlist <- peekCString cstr
  let appended = charlist ++ " example!!"
  promise <- async $ do
    putStrLn $ "The result is: " <> appended
  wait promise
  newCString appended


foreign import ccall "dynamic" ptrToFun :: FunPtr (CInt -> CInt) -> (CInt -> CInt)
foreign export ccall mappy :: Ptr CInt -> FunPtr (CInt -> CInt) -> IO (Ptr CInt)
mappy :: Ptr CInt -> FunPtr (CInt -> CInt) -> IO (Ptr CInt)
mappy inPtr funPtr = do
  list <- peekArray0 0 inPtr

  let fun = ptrToFun funPtr
  let list' = fmap fun list

  outPtr <- mallocArray (length list')
  pokeArray0 0 outPtr list'
  pure outPtr


foo :: (CInt -> CInt) -> (Int -> Int)
foo fun = fromIntegral . fun . fromIntegral
