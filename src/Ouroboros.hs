{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
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

foreign export ccall example :: CString -> IO CString
example :: CString -> IO CString 
example cstr = do
  charlist <- peekCString cstr
  let appended = charlist ++ " example!!"
  promise <- async $ do
    putStrLn $ "The result is: " <> appended
  wait promise
  newCString appended

-- | Export `reallocBytes` so the foreign (in this case Python) code
-- can allocate, reallocate and free memory
-- which can also be cleaned up by using 
-- the Foreign.Marshal.Alloc functions from within Haskell
foreign export ccall haskellRealloc :: Ptr a -> Word -> IO (Ptr a)
haskellRealloc ptr size = reallocBytes ptr (fromIntegral size)



-- type SizePrefixedArray elem = (Word, Ptr elem)

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
    list <- SizePrefixedArray.fromRawPointerNofree @[CInt] inPtr
    -- print inPtr
    -- (len, inElemsPtr) <- peek inPtr
    -- print len
    -- -- print inElemsPtr
    -- list <- peekArray (fromIntegral len) inElemsPtr
    -- -- print list

    let fun = ptrToFun funPtr
    let fun' val = alloca $ \outputParam -> do
              print outputParam
              void $ fun val outputParam
              (err, result) <- peek outputParam
              if err then 
                E.throw E.UserInterrupt 
              else 
                pure result
    list' <- mapM fun' list
    print list'

    -- outPtr <- malloc
    -- elemsPtr <- mallocArray (length list')
    -- pokeArray elemsPtr list'
    -- poke outPtr (fromIntegral $ length list', elemsPtr)
    outPtr <- SizePrefixedArray.toRawPointer list'

    cancel nagger
    pure outPtr

nag :: IO ()
nag = do
  putStrLn "Haskell is busy doing stuff in the background"
  threadDelay 1000000
  nag

printOnInterrupt exception | exception == E.UserInterrupt = do
  print "User interrupt was received!"
  throw exception
printOnInterrupt exception = do
  throw exception

foo :: (CInt -> CInt) -> (Int -> Int)
foo fun = fromIntegral . fun . fromIntegral



type ByteStr = (FatPtr Word8)
type PurgatoryFun = (Ptr ByteStr -> Ptr ByteStr -> IO Bool)

foreign import ccall "dynamic" purgatoryFunToHeavenFun :: FunPtr PurgatoryFun -> PurgatoryFun
foreign export ccall runpython :: FunPtr PurgatoryFun -> IO Bool
runpython :: FunPtr PurgatoryFun -> IO Bool
runpython funPtr = handle exceptionToBool $ do
  putStr "Haskell: Passing input "
  let fun = funPtrToByteStrFun funPtr
  let input = FatPtr.fromList ("{'a': 1}" :: ByteString)
  output <- fun input
  putStr "Haskell: Received output "
  print (FatPtr.toList output :: ByteString)
  pure True
  where
    exceptionToBool :: SomeException -> IO Bool
    exceptionToBool _ = pure False

funPtrToByteStrFun :: FunPtr PurgatoryFun -> ByteStr -> IO ByteStr
funPtrToByteStrFun funPtr =
  \inStr ->
    alloca $ \inPtr ->
      alloca $ \outPtr -> do
        poke inPtr inStr
        print inStr
        succeeded <- (purgatoryFunToHeavenFun funPtr) inPtr outPtr
        outStr <- peek outPtr
        print outStr
        if not succeeded then do
          putStrLn "Haskell: Python threw an error, rethrowing"
          -- print (FatPtr.toList outStr :: ByteString)
          throw E.UserInterrupt
        else do
          putStrLn "Haskell: Python succeeded"
          pure outStr

type InfernoFun = (ByteBox -> ByteBox -> IO ())
foreign import ccall "dynamic" fromInfernoFun :: FunPtr InfernoFun -> InfernoFun
foreign export ccall runpython2 :: FunPtr InfernoFun -> IO ()
runpython2 :: FunPtr InfernoFun -> IO ()
runpython2 funPtr = do
  let fun = ByteBox.inferno (fromInfernoFun funPtr)
  let input = "Hello, world!"
  output <- fun input
  putStr "Haskell -- The output is: "
  print output

foreign export ccall appendMessage :: InfernoFun
appendMessage :: InfernoFun
appendMessage = bytestringFunToInfernoFun (\x -> pure $ x <> " Haskell is cooler!")

foreign export ccall haskellDiv :: InfernoFun
haskellDiv :: HasCallStack => InfernoFun
haskellDiv = throwingJSONFunToInfernoFun impl
  where
    impl :: (Integer, Integer) -> IO Integer
    impl (num, denom) = 
      -- checkpointMany [Annotation.toAnnotation ("Hello" :: String)] $
      innerDiv num denom

innerDiv :: HasCallStack => Integer -> Integer -> IO Integer
innerDiv num denom = 
  if denom == 42 then 
    throw E.UserInterrupt 
  else
    pure $ num `div` denom

bytestringFunToInfernoFun :: HasCallStack => (ByteString -> IO ByteString) -> InfernoFun
bytestringFunToInfernoFun fun = 
  \inputBox -> \outputBox -> do
    input <- ByteBox.toBorrowingByteString inputBox
    output <- fun input
    ByteBox.pokeFromByteString outputBox output

foreign export ccall printJSON :: InfernoFun
printJSON :: InfernoFun
printJSON = jsonFunToInfernoFun fun
  where
    fun :: Either String Aeson.Value -> IO Aeson.Value
    fun jsonValue =
      case jsonValue of
        Left err -> do
          putStrLn $ "Error parsing JSON: " <> err
          pure Aeson.Null
        Right (value :: Aeson.Value) -> do
          putStr "Parsed JSON representation: "
          print value
          pure value

jsonFunToInfernoFun :: HasCallStack => (Aeson.FromJSON input, Aeson.ToJSON output) => (Either String input -> IO output) -> InfernoFun
jsonFunToInfernoFun fun = bytestringFunToInfernoFun (jsonFunToBytestringFun fun)

jsonFunToBytestringFun :: HasCallStack => (Aeson.FromJSON input, Aeson.ToJSON output) => (Either String input -> IO output) -> (ByteString -> IO ByteString)
jsonFunToBytestringFun fun = \inputStr -> do
  let inputEither = Aeson.eitherDecodeStrict inputStr
  outputValue <- fun inputEither
  let output = ByteString.Lazy.toStrict $ Aeson.encode outputValue
  pure output

throwingJSONFunToInfernoFun :: HasCallStack => (NFData input, Aeson.FromJSON input, NFData output, Aeson.ToJSON output) => (input -> IO output) -> InfernoFun
throwingJSONFunToInfernoFun fun = jsonFunToInfernoFun (throwingFunToJSONFun fun)

throwingFunToJSONFun :: HasCallStack => (NFData input, Aeson.FromJSON input, NFData output, Aeson.ToJSON output, HasCallStack) => (input -> IO output) -> (Either String input -> IO (Either Aeson.Value output))
throwingFunToJSONFun fun = \inputEitherLazy -> do
  inputEither <- evaluateDeep inputEitherLazy
  case inputEither of
    Left err ->
      pure $ Left $ exceptionToJSON $ exceptionWithCallStack $ toException $ Aeson.AesonException err
    Right input -> do
      outputEither <- E.try (evaluateDeep =<< fun input)
      pure $ Bifunctor.first exceptionToJSON outputEither

exceptionToJSON :: AnnotatedException SomeException -> Aeson.Value
exceptionToJSON aex@(AnnotatedException anns exception) = 
  Aeson.object 
    ["name" Aeson..= exceptionName exception
    ,"message" Aeson..= displayException exception
    ,"callstack" Aeson..= (simplifiedCallStack . getCallStack <$> annotatedExceptionCallStack aex)
    , "annotations" Aeson..= (fmap show $ nonCallstackAnnotations anns)
    ]

exceptionName :: SomeException -> String
exceptionName e | Just (asyncEx :: E.AsyncException) <- fromException e = 
  case asyncEx of
    E.StackOverflow -> "StackOverflow"
    E.HeapOverflow -> "HeapOverflow"
    E.ThreadKilled -> "ThreadKilled"
    E.UserInterrupt -> "UserInterrupt"

exceptionName (SomeException syncEx) = show $ Reflection.typeOf syncEx

nonCallstackAnnotations :: [Annotation] -> [Annotation]
nonCallstackAnnotations anns = 
  let (_ :: [CallStack], other) = Annotation.tryAnnotations anns in other

data SimpleSrcLoc = SimpleSrcLoc {file :: String, line :: Int, col :: Int}
  deriving (Show)

instance Aeson.ToJSON SimpleSrcLoc where
  toJSON SimpleSrcLoc {..} = 
    Aeson.object
    [ "file" Aeson..= file
    , "line" Aeson..= line
    , "col" Aeson..= col
    ]

simplifiedCallStack :: [(String, SrcLoc)] -> [(String, SimpleSrcLoc)]
simplifiedCallStack = fmap (\(name, loc) -> (name, simplifiedSrcLoc loc))
simplifiedSrcLoc :: SrcLoc -> SimpleSrcLoc
simplifiedSrcLoc SrcLoc{..} = 
  -- We suffix the actual filename with the package/module name for two reasons
  -- (1) so it is shown on the other side
  -- (2) so the python traceback printer cannot find the original source file;
  --     since we cannot pass column information (it tries parsing the haskell code as python code)
  --     it would highlight completely the wrong columns which is detrimental to the readability of the stacktrace.
  let prettyFile = srcLocFile <> " (" <> srcLocPackage <> ":" <> srcLocModule <> ")" in
  SimpleSrcLoc
    { file = prettyFile
    , line = srcLocStartLine
    , col = srcLocStartCol
    }
