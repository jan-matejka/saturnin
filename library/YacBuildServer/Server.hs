{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Server
    ( runYBServer
    )
where


import Prelude hiding (lookup, log, readFile)

import Control.Applicative
import Control.Concurrent.Spawn
import Control.Monad.Catch
import Control.Monad.State
import Data.Either.Combinators
import Data.Default
import Data.HashMap.Strict
import Data.Maybe
import Data.Text.Lazy hiding (head, all)
import Formatting hiding (bind)
import Network.Socket hiding (mkSocket)
import System.Directory
import System.IO hiding (readFile)
import Text.Read hiding (get)

import YacBuildServer.Jobs
import YacBuildServer.Logging
import YacBuildServer.Server.Config
import YacBuildServer.Types


runYBServer :: IO ()
runYBServer = evalStateT serve def
  where
    serve :: YBServer ()
    serve =
        ybsCloseStdin
        >> ybsReadConfig
        >>= ybsCreateWorkdir
        >>= ybsListen
        >>= ybsAccept

getConfig :: YBServer ConfigServer
getConfig =  ybssConfig <$> get


ybsCloseStdin :: YBServer ()
ybsCloseStdin = liftIO $ hClose stdin


ybsReadConfig :: YBServer (Maybe ConfigServer)
ybsReadConfig = do
    x <- liftIO readConfig
    whenLeft  x $ liftIO . logShown
    whenRight x $ \z -> get >>= \y -> put y { ybssConfig = z }
    return $ rightToMaybe x


ybsCreateWorkdir :: Maybe ConfigServer -> YBServer (Maybe ConfigServer)
ybsCreateWorkdir (Just cg) = do
    catch
        (liftIO . createDirectoryIfMissing True . fromJust $ work_dir cg)
        (liftIO . logShownPrefix p :: SomeException -> YBServer ())
    return $ Just cg
  where
    p = "Failed to create working directory"
ybsCreateWorkdir Nothing = return Nothing


ybsListen
    :: (Maybe ConfigServer)
    -> YBServer (Maybe Socket)
ybsListen (Just cg) = do
    addrinfos <- liftIO $ getAddrInfo
        (Just defaultHints {addrFamily = AF_INET})
        (listen_addr cg)
        (listen_port cg)

    let addr = head addrinfos

    sock <- liftIO $ socket (addrFamily addr) Stream defaultProtocol
    _ <- liftIO . bind sock $ addrAddress addr

    liftIO . logInfo $ format ("Listening on " % shown) addr
    _ <- liftIO $ listen sock 5

    return $ Just sock
ybsListen Nothing = return Nothing


ybsAccept :: Maybe Socket -> YBServer ()
ybsAccept (Just x) = mapM_ ((ybsHandleConnection =<<) . (liftIO . accept)) $ repeat x
ybsAccept Nothing = return ()

ybsMkJob :: (Socket, Maybe JobRequest) -> YBServer (Socket, Maybe Job)
ybsMkJob (c, Just x) = do
    ms   <- ybsSelectMachines x
    l <- getLogger x

    return . (,) c . Just $ Job
        { jobLogger = l "master"
        , remoteJobs = (\(m, h) -> mkRemoteJob x (l m) c m h) <$> ms
        , request = x
        , jobConnection = c
        }
ybsMkJob (x, _) = return $ (x, Nothing)

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing (Just _) _ = pure ()
whenNothing Nothing f = f

ybsReadJobRequest :: Socket -> YBServer (Socket, Maybe JobRequest)
ybsReadJobRequest c = do
    bytes <- liftIO $ fst3 <$> recvFrom c 1024
    let mjr = readMaybe bytes
    liftIO . whenNothing mjr
        $ logShownPrefix "failed readJobRequest " bytes
    return $ (c, mjr)

logConnection :: (Socket, SockAddr) -> YBServer Socket
logConnection (x, y) =
    (liftIO . logInfo $ format ("connected: " % shown) y)
        >> return x

ybsHandleConnection :: (Socket, SockAddr) -> YBServer ()
ybsHandleConnection x =
    logConnection x
        >>= ybsReadJobRequest
        >>= ybsMkJob
        >>= ybsDistributeJob
        >>= reportJobResult
        >>= closeConnection

ybsDistributeJob
    :: (Socket, Maybe Job)
    -> YBServer (Socket, Maybe (Job, [JobResult]))
ybsDistributeJob (s, Just x) = do
    y <- liftIO $ (,) s . Just . (,) x <$> parMapIO runRemoteJob (remoteJobs x)
    return y
ybsDistributeJob (x, Nothing) = return (x, Nothing)

reportJobResult
    :: (Socket, Maybe (Job, [JobResult]))
    -> YBServer Socket
reportJobResult (s, Just (j, xs)) = do
    let msg = format (
            "\n\n\nJob finished: " %shown% "\n" %
            "Job results: " %shown% "\n" %
            "Overal result: " %shown% "\n"
            ) (request j) xs overall

    _ <- liftIO $ (jobLogger j) msg
    _ <- liftIO $ (void . send (jobConnection j) . unpack) msg
    return s
  where
    overall = if all isPassed $ result <$> xs
              then Passed
              else Failed

reportJobResult (s, _) = return s

-- | FIXME: Unhandled failure:
-- when not all requested machines are available
ybsSelectMachines
    :: JobRequest
    -> YBServer [(MachineDescription, Hostname)]
ybsSelectMachines r =
    (filterMachines (testMachines r) . machines) <$> getConfig

closeConnection
    :: Socket
    -> YBServer ()
closeConnection c = do
    h <- liftIO $ socketToHandle c ReadWriteMode
    _ <- liftIO $ hFlush h
    _ <- liftIO $ hClose h
    return ()

getLogger :: JobRequest -> YBServer DistributedJobLogger
getLogger r = do
    jlp <- liftIO $ jobLogPath r
    _   <- liftIO $ createDirectoryIfMissing True jlp
    return $ jobLog jlp

filterMachines
    :: [MachineDescription]
    -> HashMap MachineDescription Hostname
    -> [(MachineDescription, Hostname)]
filterMachines ss xs = toList $ filterWithKey (\k _ -> elem k ss) xs
