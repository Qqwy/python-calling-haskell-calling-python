{-# LANGUAGE ForeignFunctionInterface #-}
module Ouroboros where
import qualified Data.Char as C
import Foreign.C.Types
import Foreign.C.String

import Control.Concurrent.Async (async, wait)

example :: CString -> IO CString 
example cstr = do
  charlist <- peekCString cstr
  let appended = charlist ++ " example!!"
  promise <- async $ do
    putStrLn $ "The result is: " <> appended
  wait promise
  newCString appended

foreign export ccall example :: CString -> IO CString
