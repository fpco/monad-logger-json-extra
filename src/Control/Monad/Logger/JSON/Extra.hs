{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards,
  QuasiQuotes, TemplateHaskell #-}

-- | This module provides wrappers around "Control.Monad.Logger" that make it easy to produce
-- properly JSON formatted log output.
module Control.Monad.Logger.JSON.Extra
  (
   -- * Helper transformers
   runStderrJSONLoggingT
  ,runStdoutJSONLoggingT
  ,runLoggerSetJSONLoggingT
  ,
   -- * TH logging of JSON values
   logDebugJ
  ,logInfoJ
  ,logWarnJ
  ,logErrorJ
  ,logOtherJ
  ,
   -- * TH logging of JSON values with source
   logDebugSJ
  ,logInfoSJ
  ,logWarnSJ
  ,logErrorSJ
  ,logOtherSJ
  ,
   -- * Non-TH logging of JSON values
   logDebugNJ
  ,logInfoNJ
  ,logWarnNJ
  ,logErrorNJ
  ,logOtherNJ
  ,
   -- * Non-TH logging of JSON values with source
   logDebugNSJ
  ,logInfoNSJ
  ,logWarnNSJ
  ,logErrorNSJ
  ,logOtherNSJ
  ,
   -- * Utilities for defining your own loggers
   jsonMessageLoggerSource
  ,defaultLoc
  ,formatJSONLogMessage
  ,fromLogStr
  ,newJSONTimeCache
  ,
   -- * Re-export from Control.Monad.Logger
   module Control.Monad.Logger)
  where

import Control.AutoUpdate
       (mkAutoUpdate, defaultUpdateSettings, updateAction)
import Control.Monad (when)
import Control.Monad.Logger
       hiding (runStderrLoggingT, runStdoutLoggingT,
               withChannelLogger, logDebug, logInfo, logWarn,
               logError, logOther, logDebugS, logInfoS, logWarnS, logErrorS,
               logOtherS, logDebugN, logInfoN, logWarnN, logErrorN, logOtherN,
               logDebugNS, logInfoNS, logWarnNS, logErrorNS, logOtherNS)
import Data.Aeson (ToJSON, encode)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.UnixTime (formatUnixTime, fromEpochTime)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (lift, qLocation)
import System.Log.FastLogger
       (LoggerSet, defaultBufSize, flushLogStr, fromLogStr,
        newStderrLoggerSet, newStdoutLoggerSet, pushLogStr)
import System.Posix (epochTime)
import Yesod.Core.Types (Logger, loggerDate, loggerPutStr)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

-- | Run a block using a 'MonadLogger' instance which prints to stdout.
runStdoutJSONLoggingT :: LoggingT IO a -> IO a
runStdoutJSONLoggingT = flip runLoggerSetJSONLoggingT (newStdoutLoggerSet defaultBufSize)

-- | Run a block using a 'MonadLogger' instance which prints to stderr.
runStderrJSONLoggingT :: LoggingT IO a -> IO a
runStderrJSONLoggingT = flip runLoggerSetJSONLoggingT (newStderrLoggerSet defaultBufSize)

-- | Run a block using a 'MonadLogger' instance which pushes to the given 'LoggerSet'.
-- Note: this flushes the LoggerSet after every log messages for immediate output instead of debounced.
runLoggerSetJSONLoggingT :: LoggingT IO a -> IO LoggerSet -> IO a
runLoggerSetJSONLoggingT inner mkLoggerSet = do
    getdate <- newJSONTimeCache
    loggerSet <- mkLoggerSet
    let output loc src level msg = do
            json <- formatJSONLogMessage getdate loc src level msg
            pushLogStr loggerSet json
            flushLogStr loggerSet
    runLoggingT inner output

-- | Generates a function that takes a 'ToJSON' instance and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugJ obj
logDebugJ :: Q Exp
logDebugJ = logJSON LevelDebug

-- | Generates a function that takes a 'ToJSON' instance and logs a 'LevelInfo' message. Usage:
--
-- > $logInfoJ obj
logInfoJ :: Q Exp
logInfoJ = logJSON LevelInfo

-- | Generates a function that takes a 'ToJSON' instance and logs a 'LevelWarn' message. Usage:
--
-- > $logWarnJ obj
logWarnJ :: Q Exp
logWarnJ = logJSON LevelWarn

-- | Generates a function that takes a 'ToJSON' instance and logs a 'LevelError' message. Usage:
--
-- > $logErrorJ obj
logErrorJ :: Q Exp
logErrorJ = logJSON LevelError

-- | Takes a 'Text' and generates a function that takes a 'ToJSON' instance and logs a 'LevelOther' message.
-- Usage:
--
-- > $(logOtherJ "SomeLevel") obj
logOtherJ :: T.Text -> Q Exp
logOtherJ = logJSON . LevelOther

-- | Generates a function that takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugSJ "SomeSource" obj
logDebugSJ :: Q Exp
logDebugSJ = logSourceJSON LevelDebug

-- | Generates a function that takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelInfo' message. Usage:
--
-- > $logInfoSJ "SomeSource" obj
logInfoSJ :: Q Exp
logInfoSJ = logSourceJSON LevelInfo

-- | Generates a function that takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelWarn' message. Usage:
--
-- > $logWarnSJ "SomeSource" obj
logWarnSJ :: Q Exp
logWarnSJ = logSourceJSON LevelWarn

-- | Generates a function that takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelError' message. Usage:
--
-- > $logErrorSJ "SomeSource" obj
logErrorSJ :: Q Exp
logErrorSJ = logSourceJSON LevelError

-- | Takes a 'Text' and generates a function that takes a 'LogSource' and 'ToJSON' instance and logs a
-- 'LevelOther' message. Usage:
--
-- > $(logOtherSJ "SomeLevel") "SomeSource" obj
logOtherSJ :: T.Text -> Q Exp
logOtherSJ = logSourceJSON . LevelOther

-- | Takes a 'ToJSON' instance and logs a 'LevelDebug' message. Usage:
--
-- > logDebugNJ obj
logDebugNJ
    :: (MonadLogger m, ToJSON msg)
    => msg -> m ()
logDebugNJ = logWithoutLoc LevelDebug

-- | Takes a 'ToJSON' instance and logs a 'LevelInfo' message. Usage:
--
-- > logInfoNJ obj
logInfoNJ
    :: (MonadLogger m, ToJSON msg)
    => msg -> m ()
logInfoNJ = logWithoutLoc LevelInfo

-- | Takes a 'ToJSON' instance and logs a 'LevelWarn' message. Usage:
--
-- > logWarnNJ obj
logWarnNJ
    :: (MonadLogger m, ToJSON msg)
    => msg -> m ()
logWarnNJ = logWithoutLoc LevelWarn

-- | Takes a 'ToJSON' instance and logs a 'LevelError' message. Usage:
--
-- > logErrorNJ obj
logErrorNJ
    :: (MonadLogger m, ToJSON msg)
    => msg -> m ()
logErrorNJ = logWithoutLoc LevelError

-- | Takes a 'Text' and 'ToJSON' instance and logs a 'LevelOther' message. Usage:
--
-- > logOtherNJ "SomeLevel" obj
logOtherNJ
    :: (MonadLogger m, ToJSON msg)
    => T.Text -> msg -> m ()
logOtherNJ = logWithoutLoc . LevelOther

-- | Takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelDebug' message. Usage:
--
-- > logDebugNSJ "SomeSource" obj
logDebugNSJ
    :: (MonadLogger m, ToJSON msg)
    => LogSource -> msg -> m ()
logDebugNSJ = logSourceWithoutLoc LevelDebug

-- | Takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelInfo' message. Usage:
--
-- > logInfoNSJ "SomeSource" obj
logInfoNSJ
    :: (MonadLogger m, ToJSON msg)
    => LogSource -> msg -> m ()
logInfoNSJ = logSourceWithoutLoc LevelInfo

-- | Takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelWarn' message. Usage:
--
-- > logWarnNSJ "SomeSource" obj
logWarnNSJ
    :: (MonadLogger m, ToJSON msg)
    => LogSource -> msg -> m ()
logWarnNSJ = logSourceWithoutLoc LevelWarn

-- | Takes a 'LogSource' and 'ToJSON' instance and logs a 'LevelError' message. Usage:
--
-- > logErrorNSJ "SomeSource" obj
logErrorNSJ
    :: (MonadLogger m, ToJSON msg)
    => LogSource -> msg -> m ()
logErrorNSJ = logSourceWithoutLoc LevelError

-- | Takes a 'Text', 'LogSource' and 'ToJSON' instance, and logs a 'LevelOther' message. Usage:
--
-- > logOtherNSJ "SomeLevel" "SomeSource" obj
logOtherNSJ
    :: (MonadLogger m, ToJSON msg)
    => T.Text -> LogSource -> msg -> m ()
logOtherNSJ = logSourceWithoutLoc . LevelOther

-- | Takes a 'LogLevel' and generates a function that takes a 'ToJSON' instance and logs a message
logJSON :: LogLevel -> Q Exp
logJSON level =
    [|\a ->
           monadLoggerLog $(qLocation >>= liftLoc) jsonSourceSuffix $(lift level) (encode a)|]

-- | Takes a 'LogLevel' and generates a function that takes a 'LogSource' and 'ToJSON' instance and logs a
-- message.
logSourceJSON :: LogLevel -> Q Exp
logSourceJSON level =
    [|\src msg ->
           monadLoggerLog
               $(qLocation >>= liftLoc)
               (src <> jsonSourceSuffix)
               $(lift level)
               (encode msg)|]

-- | Takes a 'LogLevel' and 'ToJSON' instance and logs a message. Usage:
logWithoutLoc
    :: (MonadLogger m, ToJSON msg)
    => LogLevel -> msg -> m ()
logWithoutLoc level = monadLoggerLog defaultLoc jsonSourceSuffix level . encode

-- | Takes a 'LogLevel', 'LogSource' and 'ToJSON' instance, and logs a message. Usage:
logSourceWithoutLoc
    :: (MonadLogger m, ToJSON msg)
    => LogLevel -> LogSource -> msg -> m ()
logSourceWithoutLoc level src = monadLoggerLog defaultLoc (src <> jsonSourceSuffix) level . encode

-- | A function appropriate for Yesod's 'Yesod.Core.messageLoggerSource' that formats with JSON.
-- The 'Logger''s date getter should use 'newJSONTimeCache' so that the dates are in the commonly
-- used ISO-8601 format .
jsonMessageLoggerSource
    :: (LogSource -> LogLevel -> IO Bool) -- ^ Check whether we should
                                          -- log this
    -> Logger
    -> Loc -- ^ position in source code
    -> LogSource
    -> LogLevel
    -> LogStr -- ^ message
    -> IO ()
jsonMessageLoggerSource ckLoggable logger loc source level msg = do
    loggable <- ckLoggable source level
    when loggable $
        formatJSONLogMessage (loggerDate logger) loc source level msg >>= loggerPutStr logger

-- | Returns and efficient date/time string getter that uses the ISO-8601 format most commonly used
-- with JSON.
newJSONTimeCache
    :: IO (IO S8.ByteString)
newJSONTimeCache =
    mkAutoUpdate
        defaultUpdateSettings
        { updateAction = epochTime >>= (formatUnixTime "%FT%TZ" . fromEpochTime)
        }

-- | Builds a JSON-formatted log message.
formatJSONLogMessage :: IO S8.ByteString -> Loc -> LogSource -> LogLevel -> LogStr -> IO LogStr
formatJSONLogMessage getdate Loc{..} source0 level message0 = do
    now <- getdate
    -- This "manually" builds the JSON metadata instead of using Aeson to encode the whole object
    -- for efficiency so that the whole 'message' does not need to be decoded and then re-encoded.
    return $
        mconcat
            [ "{\"date\":"
            , encode' (S8.unpack now)
            , ",\"level\":"
            , encode' defaultLogLevelStr
            , (if T.null source'
                   then ""
                   else ",\"source\":" <> encode' source')
            , ",\"message\":"
            , message'
            , (if loc_package == "<unknown>"
                   then ""
                   else ",\"location\":" <> encode' fileLocStr)
            , "}\n"]
  where
    encode'
        :: ToJSON a
        => a -> LogStr
    encode' = toLogStr . encode
    (message',source') =
        if jsonSourceSuffix `T.isSuffixOf` source0
            then (message0, T.dropEnd jsonSourceSuffixLength source0)
            else (encode' (S8.unpack (fromLogStr message0)), source0)
    fileLocStr =
        concat
            [ loc_package
            , ':' : loc_module
            , ' ' : loc_filename
            , ':' : show (fst (loc_start))
            , ':' : show (snd (loc_start))]
    defaultLogLevelStr =
        case level of
            LevelOther t -> T.unpack t
            _ -> map toLower $ drop 5 (show level)

-- | The default "unknown" source code location.
defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)

-- | String to append to the 'LogSource' to indicate that the message is JSON formatted.
jsonSourceSuffix
    :: T.Text
jsonSourceSuffix = "%JSON"

-- | Length of the JSON 'LogSource' suffix.
jsonSourceSuffixLength :: Int
jsonSourceSuffixLength = T.length jsonSourceSuffix
