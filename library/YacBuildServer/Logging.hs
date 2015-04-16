{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Logging
    ( getJobLogger
    , Logger
    , DistributedJobLogger
    , logServer
    )
where

import Data.Monoid
import Data.Text.Lazy
import System.Directory
import System.FilePath.Posix
import System.IO

import YacBuildServer.Server.Config

serverLog :: FilePath
serverLog = "/var/log/ybs.log"

jobLogs :: FilePath
jobLogs = "/var/lib/ybs/job-logs"

logServer :: Text -> IO ()
logServer x = logToLog x >> logToStdout x
  where
    logToLog = appendFile serverLog . unpack
    logToStdout = hPutStr stderr . unpack

jobLog
    :: FilePath -- job's base logging path
    -> MachineDescription
    -> Text
    -> IO ()
jobLog p m msg = appendFile (p </> (m <> ".txt")) $ unpack msg

getJobLogger :: JobID -> IO DistributedJobLogger
getJobLogger (JobID x) = do
    let p = jobLogs </> (show x)
    _   <- createDirectoryIfMissing True p
    return $ jobLog p

type Logger = Text -> IO ()
type DistributedJobLogger = FilePath -> Text -> IO ()
