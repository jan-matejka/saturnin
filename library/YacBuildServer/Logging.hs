{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Logging
    ( logError
    , logInfo
    , jobLogPath
    , jobLog
    , Logger
    , DistributedJobLogger
    )
where

import Formatting
import Data.Char
import Data.Monoid
import Data.Text.Lazy
import Data.Time.Clock
import Data.Time.Format
import System.FilePath.Posix
import System.IO
import System.Locale

import YacBuildServer.Types

serverLog :: FilePath
serverLog = "/var/lib/ybs.log"

_log :: String -> IO ()
_log = appendFile serverLog

type Level   = Text
type Message = Text

_msg :: Level -> Message -> String
_msg l m = unpack $ format (text % ": " % text % "\n") l m

logError :: Text -> IO ()
logError x = do
    _log   m
    logStd m
  where
    m = _msg "error" x
    logStd = hPutStr stderr

logInfo :: Text -> IO ()
logInfo x = do
    _log   m
    logStd m
  where
    m = _msg "info" x
    logStd = putStr


jobLogs :: FilePath
jobLogs = "/var/lib/ybs/job-logs"

jobLogPath :: BuildRequest -> IO FilePath
jobLogPath r = do
    t <- getCurrentTime
    return
        $   jobLogs
        </> (dirname r)
        </> fmt t
  where
    fmt = formatTime defaultTimeLocale "%F.%T.%Z"

    dirname :: BuildRequest -> String
    dirname (GitBuildRequest u h) = (san u) <> "-" <> (san h)

    san :: String -> String
    san = fmap (\x -> if isAlphaNum x then x else '_')

jobLog
    :: FilePath -- jobs base logging path
    -> MachineDescription
    -> Message
    -> IO ()
jobLog p m msg = appendFile (p </> (m <> ".txt")) $ unpack msg

type Logger = Text -> IO ()
type DistributedJobLogger = FilePath -> Text -> IO ()
