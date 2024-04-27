{-# LANGUAGE ForeignFunctionInterface #-}
module Ouroboros where
import qualified Data.Char as C
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Control.Monad (forever)
import Control.Concurrent (threadDelay, yield)
import Control.Concurrent.Async (async, wait, race, withAsync, cancel, forConcurrently)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SVector
import Control.Exception

import Control.Exception as E
import Control.Concurrent
import System.Posix.Signals


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
mappy inPtr funPtr = handle printOnInterrupt $ do
  tid <- myThreadId
  -- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing

  withAsync nag $ \nagger -> do
    list <- peekArray0 0 inPtr
    -- print list

    let fun = ptrToFun funPtr 
    let fun' val = do
              let res = fun val
              errno <- res `seq` getErrno
              if errno == eINTR then throw UserInterrupt else pure res
    list' <- mapM fun' list
    print list'

    outPtr <- mallocArray (length list')
    pokeArray0 0 outPtr list'

    cancel nagger
    pure outPtr

nag :: IO ()
nag = do
  putStrLn "Haskell is busy doing stuff in the background"
  threadDelay 1000000
  nag

printOnInterrupt exception | exception == UserInterrupt = do
  print exception 
  throw exception
printOnInterrupt exception = do
  setErrNo eINTR
  -- throw exception

foo :: (CInt -> CInt) -> (Int -> Int)
foo fun = fromIntegral . fun . fromIntegral
