{-# LANGUAGE ForeignFunctionInterface #-}
module Ouroboros where
import qualified Data.Char as C
import Foreign.C.Types
import Foreign.C.String

example :: CString -> IO CString 
example cstr = do
  charlist <- peekCString cstr
  let appended = charlist ++ " example!!"
  newCString appended

foreign export ccall example :: CString -> IO CString
