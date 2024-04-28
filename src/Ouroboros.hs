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
import Foreign.Storable.Tuple


foreign export ccall example :: CString -> IO CString
example :: CString -> IO CString 
example cstr = do
  charlist <- peekCString cstr
  let appended = charlist ++ " example!!"
  promise <- async $ do
    putStrLn $ "The result is: " <> appended
  wait promise
  newCString appended

type SizePrefixedArray elem = (Word, Ptr elem)

-- | Model a sum type as an (error, value) product type, as it is easier to use it from Python that way.
-- If the bool is true, it contains an error and the second element might be garbage.
-- If the bool is false, `value` can be read.
type ResultTuple a = (Bool, a)
type IntToIntCallback = (CInt -> Ptr (ResultTuple CInt) -> IO ())

foreign import ccall "dynamic" ptrToFun :: FunPtr IntToIntCallback -> IntToIntCallback
foreign export ccall mappy :: Ptr (SizePrefixedArray CInt) -> FunPtr IntToIntCallback -> IO (Ptr (SizePrefixedArray CInt))
mappy :: Ptr (SizePrefixedArray CInt) -> FunPtr IntToIntCallback -> IO (Ptr (SizePrefixedArray CInt))
mappy inPtr funPtr = handle printOnInterrupt $ do
  tid <- myThreadId
  -- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing

  withAsync nag $ \nagger -> do
    print inPtr
    (len, inElemsPtr) <- peek inPtr
    print len
    -- print inElemsPtr
    list <- peekArray (fromIntegral len) inElemsPtr
    -- print list

    let fun = ptrToFun funPtr
    let fun' val = alloca $ \outputParam -> do
              print outputParam
              void $ fun val outputParam
              (err, result) <- peek outputParam
              if err then 
                throw UserInterrupt 
              else 
                pure result
    list' <- mapM fun' list
    print list'

    outPtr <- malloc
    elemsPtr <- mallocArray (length list')
    pokeArray elemsPtr list'
    poke outPtr (fromIntegral $ length list', elemsPtr)

    cancel nagger
    pure outPtr

nag :: IO ()
nag = do
  putStrLn "Haskell is busy doing stuff in the background"
  threadDelay 1000000
  nag

printOnInterrupt exception | exception == UserInterrupt = do
  print "User interrupt was received!"
  throw exception
printOnInterrupt exception = do
  throw exception

foo :: (CInt -> CInt) -> (Int -> Int)
foo fun = fromIntegral . fun . fromIntegral
