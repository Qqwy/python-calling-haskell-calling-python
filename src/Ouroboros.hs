{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
module Ouroboros where
import Foreign.Ptr
import Foreign.Marshal

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import ByteBox (ByteBox)
import ByteBox qualified
import Data.Aeson qualified as Aeson
import qualified Data.Bifunctor as Bifunctor
import Control.Exception.Annotated.UnliftIO
import UnliftIO.Exception (evaluateDeep, handle)
import Control.DeepSeq (NFData)
import GHC.Stack
import SerializedExceptions

-- | Export `reallocBytes` so the foreign (in this case Python) code
-- can allocate, reallocate and free memory
-- which can also be cleaned up by using 
-- the Foreign.Marshal.Alloc functions from within Haskell
--
-- NOTE: It is paramount to export this function rather than using 'whatever malloc/realloc/free Python has'
-- because there is no guarantee that it's built/linked against the same allocator library.
foreign export ccall haskellRealloc :: Ptr a -> Word -> IO (Ptr a)
haskellRealloc :: Ptr a -> Word -> IO (Ptr a)
haskellRealloc ptr size = reallocBytes ptr (fromIntegral size)


type InfernoFun = (ByteBox -> ByteBox -> IO ())

-- | Convert remote closures to Haskell closures

foreign import ccall "dynamic" fromInfernoFunPtr :: FunPtr InfernoFun -> InfernoFun

-- | Convert Haskell closures to remote closures
foreign import ccall "wrapper" toInfernoFunPtr :: InfernoFun -> IO (FunPtr InfernoFun)

bytestringFunToInfernoFun :: HasCallStack => (ByteString -> IO ByteString) -> InfernoFun
bytestringFunToInfernoFun fun = (\inBox -> \outBox -> ByteBox.withBorrowingByteString inBox fun >>= ByteBox.pokeFromByteString outBox)

infernoClosureToBytestringClosure :: HasCallStack => InfernoFun -> (ByteString -> IO ByteString)
infernoClosureToBytestringClosure closure = \inStr ->
  ByteBox.withByteStringAsByteBox inStr $ \inBox ->
    ByteBox.alloca $ \outBox -> do
      () <- closure inBox outBox
      ByteBox.copyToByteString outBox


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

newtype OutputParseException = OutputParseException String
  deriving newtype Show
  deriving NFData

instance Exception OutputParseException


bytestringClosureToJSONClosure :: (HasCallStack, Aeson.ToJSON input, Aeson.FromJSON output) => (ByteString -> IO ByteString) -> (input -> IO (Either OutputParseException output))
bytestringClosureToJSONClosure closure = \input ->
  input & encode & closure <&> decode
  where
    decode = Bifunctor.first OutputParseException . Aeson.eitherDecodeStrict
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

jsonClosureToThrowingJSONClosure 
  :: (HasCallStack, NFData output, Aeson.ToJSON input, Aeson.FromJSON output) 
  => (input -> IO (Either OutputParseException (Either SerializableException output))) 
  -> (input -> IO output)
jsonClosureToThrowingJSONClosure closure = \input -> do
  output <- closure input
  case output of
    Left err -> throw err -- parsing callback output failed
    Right (Left err) -> rethrowPythonError err -- Running callback failed
    Right (Right val) -> pure val


rethrowPythonError :: (HasCallStack, Exception error) => error -> IO output
rethrowPythonError err = throw err -- TODO


throwingJSONFunToInfernoFun :: HasCallStack => (NFData input, Aeson.FromJSON input, NFData output, Aeson.ToJSON output) => (input -> IO output) -> InfernoFun
throwingJSONFunToInfernoFun fun = fun & throwingJSONFunToJSONFun & jsonFunToInfernoFun 

newtype InfernoClosure input output = InfernoClosure Word
  deriving (Show, Aeson.FromJSON, Aeson.ToJSON, NFData)

fromInfernoClosureRaw :: InfernoClosure input output -> InfernoFun
fromInfernoClosureRaw (InfernoClosure funPtrAddr) = 
  funPtrAddr
  & WordPtr
  & wordPtrToPtr 
  & castPtrToFunPtr
  & fromInfernoFunPtr

fromInfernoClosure :: (HasCallStack, Aeson.ToJSON input, Aeson.FromJSON output, NFData output) => InfernoClosure input output -> (input -> IO output)
fromInfernoClosure infernoClosure = 
  infernoClosure
  & fromInfernoClosureRaw
  & infernoClosureToBytestringClosure
  & bytestringClosureToJSONClosure
  & jsonClosureToThrowingJSONClosure

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

foreign export ccall mappy :: InfernoFun
mappy :: HasCallStack => InfernoFun
mappy = 
  throwingJSONFunToInfernoFun impl
  where
    impl :: (InfernoClosure Integer Integer, [Integer]) -> IO [Integer]
    impl (closurePtr, elems) = do
      let closure = fromInfernoClosure closurePtr
      mapM closure elems
