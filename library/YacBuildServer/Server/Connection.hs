{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Server.Connection
    ( handleConnection
    )
where

import Prelude hiding (lookup, log, readFile)

import Control.Applicative
import Control.Concurrent.Spawn
import Control.Concurrent.STM
import Control.Monad.State
import Data.HashMap.Strict
import Data.Text.Lazy hiding (head, all)
import Data.Time.Clock
import Formatting hiding (bind)
import Network.Socket
import System.IO hiding (readFile)
import Text.Read hiding (get, lift)

import YacBuildServer.Jobs
import YacBuildServer.Logging
import YacBuildServer.Server.Config
import YacBuildServer.Types

getServerState :: JobRequestListenerConnectionHandler YBServerState
getServerState = lift get

getConfig :: JobRequestListenerConnectionHandler ConfigServer
getConfig = ybssConfig <$> getServerState

handleConnection :: (Socket, SockAddr) -> YBServer ()
handleConnection x = evalStateT handle' x
  where
    handle' = logConnection
        >>  readJobRequest
        >>= mkJob
        >>= logJobStart
        >>= distributeJob
        >>= reportJobResult
        >> closeConnection

logConnection :: JobRequestListenerConnectionHandler ()
logConnection = do
    x <- snd <$> get
    void . liftIO . logInfo $ format ("connected: " % shown) x

readJobRequest :: JobRequestListenerConnectionHandler (Maybe JobRequest)
readJobRequest = do
    c <- fst <$> get
    bytes <- liftIO $ fst3 <$> recvFrom c 1024
    let mjr = readMaybe bytes
    liftIO . whenNothing mjr
        $ logShownPrefix "failed readJobRequest " bytes
    return mjr

mkJob
    :: Maybe JobRequest
    -> JobRequestListenerConnectionHandler (Maybe Job)
mkJob (Just x) = do
    ms   <- selectMachines x
    jid  <- getJobID
    l    <- liftIO $ getJobLogger jid
    c    <- fst <$> get

    return . Just $ Job
        { jobLogger = l "master"
        , remoteJobs = (\(m, h) -> mkRemoteJob x (l m) c m h) <$> ms
        , request = x
        , jobConnection = c
        , jobID = jid
        }
mkJob Nothing = return Nothing

logJobStart
    :: Maybe Job
    -> JobRequestListenerConnectionHandler (Maybe Job)
logJobStart (Just j) = do
    t <- liftIO getCurrentTime
    _ <- liftIO . (jobLogger j)
        $ format (shown% " starting job " %shown) t j
    return $ Just j
logJobStart x = return x

distributeJob
    :: Maybe Job
    -> JobRequestListenerConnectionHandler (Maybe (Job, [JobResult]))
distributeJob (Just x) = do
    y <- liftIO $ Just . (,) x <$> parMapIO runRemoteJob (remoteJobs x)
    return y
distributeJob Nothing = return Nothing

reportJobResult
    :: Maybe (Job, [JobResult])
    -> JobRequestListenerConnectionHandler ()
reportJobResult (Just (j, xs)) = do
    let msg = format (
            "\n\n\nJob finished: " %shown% "\n" %
            "Job results: " %shown% "\n" %
            "Overal result: " %shown% "\n"
            ) (request j) xs overall

    _ <- liftIO $ (jobLogger j) msg
    _ <- liftIO $ (void . send (jobConnection j) . unpack) msg
    return ()
  where
    overall = if all isPassed $ result <$> xs
              then Passed
              else Failed

reportJobResult Nothing = return ()

closeConnection :: JobRequestListenerConnectionHandler ()
closeConnection = do
    c <- fst <$> get
    h <- liftIO $ socketToHandle c ReadWriteMode
    _ <- liftIO $ hFlush h
    _ <- liftIO $ hClose h
    return ()

getJobID :: JobRequestListenerConnectionHandler JobID
getJobID = do
    new <- pState <$> getServerState
        >>= liftIO . atomically . getBumped
    _ <- liftIO $ writePState new
    return $ lastJobID new
  where
    getBumped :: TVar YBServerPersistentState -> STM (YBServerPersistentState)
    getBumped x = do
        old <- readTVar x
        let new = old { lastJobID = succ $ lastJobID old }
        _ <- writeTVar x new
        return new

-- | FIXME: Unhandled failure:
-- when not all requested machines are available
selectMachines
    :: JobRequest
    -> JobRequestListenerConnectionHandler [(MachineDescription, Hostname)]
selectMachines r =
    (filterMachines (testMachines r) . machines) <$> getConfig

filterMachines
    :: [MachineDescription]
    -> HashMap MachineDescription Hostname
    -> [(MachineDescription, Hostname)]
filterMachines ss xs = toList $ filterWithKey (\k _ -> elem k ss) xs

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing (Just _) _ = pure ()
whenNothing Nothing f = f
