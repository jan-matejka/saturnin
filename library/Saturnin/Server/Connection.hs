{-# LANGUAGE OverloadedStrings #-}
module Saturnin.Server.Connection
    ( handleConnection
    )
where

import Prelude hiding (lookup, log, readFile)

import Control.Applicative
import Control.Arrow
import Control.Concurrent.Spawn
import Control.Concurrent.STM
import Control.Monad.State
import Data.HashMap.Strict
import Data.Text.Lazy hiding (head, all, length)
import Data.Time.Clock
import Formatting
import Network.Socket
import System.IO hiding (readFile)
import Text.Read hiding (get, lift)

import Saturnin.Jobs
import Saturnin.Logging
import Saturnin.Server.Config
import Saturnin.Types

-- getServerState :: JobRequestListenerConnectionHandler YBServerState
-- getServerState = lift get

--getConfig :: JobRequestListenerConnectionHandler ConfigServer
--getConfig = ybssConfig <$> getServerState

readBytes :: JobRequestListenerConnectionHandler String
readBytes = (\x -> liftIO $ fst3 <$> recvFrom x 1024) =<< (fst <$> get)

-- | Log to both log file and the client connection
logBoth :: Job -> Text -> JobRequestListenerConnectionHandler ()
logBoth j x = logToConnection x >> (logJob j x)

logJob :: Job -> Text -> JobRequestListenerConnectionHandler ()
logJob j x = do
    l <- liftIO . getJobLogger $ jobID j
    liftIO $ l "master" x

-- | Log to both server stderr and client connection
logToServerAndConn :: Text -> JobRequestListenerConnectionHandler ()
logToServerAndConn x = logToConnection x >> (lift $ logInfo x)

handleConnection :: (Socket, SockAddr) -> YBServer ()
handleConnection = evalStateT handle'
  where
    handle' = logClientConnected
        >>  readJobRequest
        >>= mkJob
        >>= logJobStart
        >>= distributeJob
        >>= returnMachines
        >>= reportJobResult
        >> reportFreeMachines
        >> closeConnection
        >> logClientDisconnected

logClientConnected :: JobRequestListenerConnectionHandler ()
logClientConnected = do
    addr <- snd <$> get
    t    <- liftIO getCurrentTime
    lift . logInfo $ format (shown % " connected: " % shown) t addr

logClientDisconnected :: JobRequestListenerConnectionHandler ()
logClientDisconnected =  do
    addr <- snd <$> get
    t    <- liftIO getCurrentTime
    lift . logInfo $ format (shown % " disconnected: " % shown) t addr

readJobRequest :: JobRequestListenerConnectionHandler (Maybe JobRequest)
readJobRequest = do
    bytes <- readBytes
    let mjr = readMaybe bytes
    whenNothing mjr . logToServerAndConn
        $ format ("failed to read JobRequest: " % shown) bytes
    return mjr

mkJob
    :: Maybe JobRequest
    -> JobRequestListenerConnectionHandler (Maybe Job)
mkJob (Just x) = do
    ms   <- selectMachines x
    whenNothing ms $
        logToServerAndConn "Unable to select all requested machines"

    mk ms
  where
    mk :: Maybe [(MachineDescription, Hostname)] -> JobRequestListenerConnectionHandler (Maybe Job)
    mk (Just ms) = do
        jid <- getJobID
        return . Just $ Job
            { remoteJobs = uncurry (mkRemoteJob x) <$> ms
            , request = x
            , jobID = jid
            }
    mk Nothing = return Nothing
mkJob Nothing = return Nothing

logJobStart
    :: Maybe Job
    -> JobRequestListenerConnectionHandler (Maybe Job)
logJobStart (Just j) = do
    t <- liftIO getCurrentTime
    logBoth j $ format (shown% " starting job " %shown) t j
    return $ Just j
logJobStart x = return x

distributeJob
    :: Maybe Job
    -> JobRequestListenerConnectionHandler (Maybe (Job, [JobResult]))
distributeJob (Just x) = do
    baseLogger <- liftIO . getJobLogger $ jobID x
    c <- fst <$> get
    rs <- liftIO $ parMapIO runRemoteJob (rJobs x baseLogger $ logToConnection' c)
    return $ Just (x, rs)
  where
    rJobs :: Job -> DistributedJobLogger -> Logger -> [RemoteJobRunnerState]
    rJobs j l cL =
        (\y -> RemoteJobRunnerState y (l $ jobMachine y) cL) <$> (remoteJobs j)

distributeJob Nothing = return Nothing

returnMachines
    :: Maybe (Job, [JobResult])
    -> JobRequestListenerConnectionHandler (Maybe (Job, [JobResult]))
returnMachines (x @ (Just (j, _))) = do
    ts <- lift get
    let returning = fromList $ (jobMachine &&& jobHost) <$> remoteJobs j
    liftIO . atomically $ do
        s <- readTVar ts
        let old = freeMachines s
        writeTVar ts $ s { freeMachines = old `union` returning }
    return x
returnMachines Nothing = return Nothing

reportJobResult
    :: Maybe (Job, [JobResult])
    -> JobRequestListenerConnectionHandler ()
reportJobResult (Just (j, xs)) = do
    logBoth j $ format (
        "\n\n\nJob finished: " %shown% "\n" %
        "Job results: " %shown% "\n" %
        "Overal result: " %shown% "\n"
        ) (request j) xs overall
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
    ts <- lift get
    ps <- liftIO . atomically $ do
        s <- readTVar ts
        let new = s { pState = bumpJobID $ pState s }
        writeTVar ts new
        return $ pState new

    liftIO $ writePState ps
    return $ lastJobID ps

reportFreeMachines :: JobRequestListenerConnectionHandler ()
reportFreeMachines = lift get
    >>= (freeMachines <$>) . liftIO . atomically . readTVar
    >>= lift . logInfo . format ("free machines: "%shown)

-- | Returns Nothing if all the request machines were not found
-- otherwise removes the taken machines the freeMachines in
-- YBServerState and returns Just the taken machines
selectMachines
    :: JobRequest
    -> JobRequestListenerConnectionHandler (Maybe [(MachineDescription, Hostname)])
selectMachines r = do
    ts <- lift get
    liftIO . atomically $ do
        s <- readTVar ts
        let requested = testMachines r
            free      = freeMachines s
            found     = filterMachines requested free

        if length found /= length requested
        then return Nothing
        else writeTVar ts
            (s { freeMachines = difference free $ fromList found})
            >> (return $ Just found)

filterMachines
    :: [MachineDescription]
    -> HashMap MachineDescription Hostname
    -> [(MachineDescription, Hostname)]
filterMachines ss xs = toList $ filterWithKey (\k _ -> elem k ss) xs

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing (Just _) _ = pure ()
whenNothing Nothing f = f
