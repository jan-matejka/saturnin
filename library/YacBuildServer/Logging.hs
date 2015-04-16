{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Logging
    ( logError
    , logInfo
    , logShown
    , logShownPrefix
    , getJobLogger
    , Logger
    , DistributedJobLogger
    , Message
    )
where

import Formatting
import Data.Monoid
import Data.Text.Lazy
import System.Directory
import System.FilePath.Posix
import System.IO

import YacBuildServer.Types

serverLog :: FilePath
serverLog = "/var/log/ybs.log"

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

logShown :: Show x => x -> IO ()
logShown = logError . format shown

logShownPrefix :: Show x => Text -> x -> IO ()
logShownPrefix p x = logError $ format (text % shown) p x

jobLogs :: FilePath
jobLogs = "/var/lib/ybs/job-logs"

jobLog
    :: FilePath -- jobs base logging path
    -> MachineDescription
    -> Message
    -> IO ()
jobLog p m msg = appendFile (p </> (m <> ".txt")) $ unpack msg

getJobLogger :: JobID -> IO DistributedJobLogger
getJobLogger (JobID x) = do
    let p = jobLogs </> (show x)
    _   <- createDirectoryIfMissing True p
    return $ jobLog p

type Logger = Text -> IO ()
type DistributedJobLogger = FilePath -> Text -> IO ()
