{-# LANGUAGE OverloadedStrings #-}
module Saturnin.Logging
    ( getJobLogger
    , Logger
    , DistributedJobLogger
    )
where

import Data.Monoid
import Data.Text.Lazy
import System.Directory
import System.FilePath.Posix

import Saturnin.Server.Config

jobLogs :: FilePath
jobLogs = "/var/lib/ybs/job-logs"

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
