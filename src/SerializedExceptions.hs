{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
module SerializedExceptions where
import qualified Control.Exception as E
import Control.Exception.Annotated (AnnotatedException)
import Control.Exception.Annotated.UnliftIO
import UnliftIO.Exception (evaluateDeep, handle)
import Control.DeepSeq (NFData)
import GHC.Stack
import qualified Data.Annotation as Annotation
import Data.Maybe (maybeToList)
import Data.Aeson qualified as Aeson
import Data.Typeable as Typeable

data SerializableException = SerializableException (AnnotatedException SomeException)
  deriving (Show)

instance Exception SerializableException

data PythonException = PythonException 
  { name :: String
  , message :: String
  , callstack :: [(String, SimpleSrcLoc)]
  , annotations :: [String]
  }
  deriving (Show)

instance Exception PythonException

instance Aeson.ToJSON SerializableException where
  toJSON (SerializableException aex@(AnnotatedException anns exception)) = 
    Aeson.object 
      ["name" Aeson..= exceptionName exception
      ,"message" Aeson..= displayException exception
      ,"callstack" Aeson..= (simplifiedCallStack . getCallStack <$> annotatedExceptionCallStack aex)
      , "annotations" Aeson..= (fmap show $ nonCallstackAnnotations anns)
      ]

instance Aeson.FromJSON SerializableException where
  parseJSON = Aeson.withObject "SerializableException" $ \o -> do
    name <- o Aeson..: "name"
    message <- o Aeson..: "message"
    callstack <- o Aeson..: "callstack"
    annotations <- o Aeson..: "annotations"
    let exception = PythonException{..}
    pure undefined
    -- pure $ SerializableException $ Annotated.toException


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

exceptionName (SomeException syncEx) = show $ Typeable.typeOf syncEx

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

instance Aeson.FromJSON SimpleSrcLoc where
  parseJSON = 
    Aeson.withObject "SimpleSrcLoc" $ \o -> do
      file <- o Aeson..: "file"
      line <- o Aeson..: "line"
      col <- o Aeson..: "col"
      pure SimpleSrcLoc{..}

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
