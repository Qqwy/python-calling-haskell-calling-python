{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
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
-- import Control.Exception

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Control.Exception as E
import Control.Concurrent
import System.Posix.Signals
import Foreign.Storable.Tuple
import SizePrefixedArray (SizePrefixedArray)
import SizePrefixedArray qualified
import FatPtr (FatPtr)
import FatPtr qualified
import Data.Word
import GHC.IsList (IsList, Item)
import GHC.IsList qualified as IsList
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as ByteString.Lazy
import ByteBox (ByteBox)
import ByteBox qualified
import Data.Aeson qualified as Aeson
import qualified Type.Reflection as Reflection
import qualified Data.Bifunctor as Bifunctor
import Control.Exception.Annotated (AnnotatedException)
import Control.Exception.Annotated.UnliftIO
import UnliftIO.Exception (evaluateDeep, handle)
import Control.DeepSeq (NFData)
import GHC.Stack
import qualified Data.Annotation as Annotation
import Data.Maybe (maybeToList)
import SerializedExceptions

-- | Export `reallocBytes` so the foreign (in this case Python) code
-- can allocate, reallocate and free memory
-- which can also be cleaned up by using 
-- the Foreign.Marshal.Alloc functions from within Haskell
--
-- NOTE: It is paramount to export this function rather than using 'whatever malloc/realloc/free Python has'
-- because there is no guarantee that it's built/linked against the same allocator library.
foreign export ccall haskellRealloc :: Ptr a -> Word -> IO (Ptr a)
haskellRealloc ptr size = reallocBytes ptr (fromIntegral size)


type InfernoFun = (ByteBox -> ByteBox -> IO ())
foreign import ccall "dynamic" fromInfernoFun :: FunPtr InfernoFun -> InfernoFun

bytestringFunToInfernoFun :: HasCallStack => (ByteString -> IO ByteString) -> InfernoFun
bytestringFunToInfernoFun fun = (\inBox -> \outBox -> ByteBox.withBorrowingByteString inBox fun >>= ByteBox.pokeFromByteString outBox)

newtype InputParseException = InputParseException String
  deriving newtype Show
  deriving NFData

instance Exception InputParseException

jsonFunToBytestringFun :: HasCallStack => (Aeson.FromJSON input, Aeson.ToJSON output) => (Either InputParseException input -> IO output) -> (ByteString -> IO ByteString)
jsonFunToBytestringFun fun = \inputStr ->
  inputStr & decode & fun <&> encode
    where
      decode = Bifunctor.first InputParseException . Aeson.eitherDecodeStrict
      encode = ByteString.Lazy.toStrict . Aeson.encode

throwingJSONFunToJSONFun :: (HasCallStack, NFData input, Aeson.FromJSON input, NFData output, Aeson.ToJSON output) => (input -> IO output) -> (Either InputParseException input -> IO (Either Aeson.Value output))
throwingJSONFunToJSONFun fun = \inputEither ->
  let inner = 
        case inputEither of
          Left err -> throw err
          Right input -> Right <$> fun input
  in
  (inner >>= evaluateDeep)
  & checkpointCallStack 
  & handle (pure . Left . SerializedExceptions.exceptionToJSON)

jsonFunToInfernoFun :: HasCallStack => (Aeson.FromJSON input, Aeson.ToJSON output) => (Either InputParseException input -> IO output) -> InfernoFun
jsonFunToInfernoFun fun = fun & jsonFunToBytestringFun & bytestringFunToInfernoFun

throwingJSONFunToInfernoFun :: HasCallStack => (NFData input, Aeson.FromJSON input, NFData output, Aeson.ToJSON output) => (input -> IO output) -> InfernoFun
throwingJSONFunToInfernoFun fun = fun & throwingJSONFunToJSONFun & jsonFunToInfernoFun 

-- Examples:


foreign export ccall haskellDiv :: InfernoFun
haskellDiv :: HasCallStack => InfernoFun
haskellDiv = throwingJSONFunToInfernoFun impl
  where
    impl :: (Integer, Integer) -> IO Integer
    impl (num, denom) = 
      -- checkpointMany [Annotation.toAnnotation ("Hello" :: String)] $ evaluateDeep =<<
      innerDiv num denom

innerDiv :: HasCallStack => Integer -> Integer -> IO Integer
innerDiv num denom =
  if denom == 42 then 
    throw E.UserInterrupt 
  else
    pure $ num `div` denom
