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
import Data.Monoid ((<>))
import Data.Text.Lazy hiding (head)
import Formatting hiding (bind)
import Network.Socket hiding (mkSocket)
import System.Directory
import System.IO hiding (readFile)
import System.IO.Temp

import YacBuildServer.Jobs
import YacBuildServer.Logging
import YacBuildServer.Server.Config
import YacBuildServer.Types
import YacBuildServer.Ybs
import YacBuildServer.Git


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
ybsAccept (Just x) = mapM_ ((handle' =<<) . accept') $ repeat x
  where
    accept' :: Socket -> YBServer Socket
    accept' sock = do
        (conn, addr) <- liftIO $ accept sock
        liftIO . logInfo $ format ("connected: " % shown) addr
        return conn

    handle' :: Socket -> YBServer ()
    handle' conn = catch
        (liftIO (readJob conn) >>= processJob conn)
        (liftIO . logShownPrefix "handleConnection: " :: SomeException -> YBServer ())

    readJob :: Socket -> IO JobRequest
    readJob c =  (read . fst3) <$> recvFrom c 1024

ybsAccept Nothing = return ()


ybsReadJobConfig
    :: Socket
    -> JobRequest
    -> YbsLogger
    -> YBServer (Maybe YbsConfig)
ybsReadJobConfig c r lgr = do
    rcg <- liftIO . readConfigFromRepo $ dataSource r
    whenLeft rcg $ \x -> do
        _ <- liftIO $ send c (show x)
        lgr "master" . pack $ show x
    return $ rightToMaybe rcg


distributeJob
    :: JobRequest
    -> DistributedJobLogger
    -> Maybe YbsConfig
    -> YBServer [Either SomeException JobResult]
distributeJob r lgr (Just rcg) = do
    xs <- selectMachines (machineDescription rcg)
    case xs of
        Left  x -> liftIO (lgr "master" x) >> return []
        Right x -> liftIO $ parMapIO distributeOne x
  where
    distributeOne x = catch
        ((Right <$>) . (liftIO . remoteProcess r) =<< logJobStart x)
        (return . Left)


    logJobStart
        :: (MachineDescription, Hostname)
        -> IO (MachineDescription, Hostname)
    logJobStart (md, h) = do
        lgr "master" $ format
            ( "Starting: " % shown
            % " for " % string
            % " at " % string
            % "\n"
            ) r md h
        return (md, h)

distributeJob _ _ Nothing = return []


-- | FIXME: Unhandled failure:
-- when not all requested machines are available
selectMachines
    :: [MachineDescription]
    -> YBServer (Either Text [(MachineDescription, Hostname)])
selectMachines requested =
    Right . filterMachines requested . machines <$> getConfig


logJobResults
    :: Socket
    -> YbsLogger
    -> [Either SomeException JobResult]
    -> YBServer ()
logJobResults c lgr xs = do
    mapM_ log' xs
    void . liftIO . send c $ (show xs <> "\n")
    return ()
  where
    log' :: Either SomeException JobResult -> YBServer ()
    log' (Left e)                  = lgr "master" . pack $ show e
    log' (Right (TestResult m cs)) = lgr m        . pack $ show cs


closeConnection
    :: Socket
    -> YBServer ()
closeConnection c = do
    h <- liftIO $ socketToHandle c ReadWriteMode
    _ <- liftIO $ hFlush h
    _ <- liftIO $ hClose h
    return ()

-- | The loggers are the same, only fst one is YBServer () and snd one is
-- pure IO ()
getLogger :: JobRequest -> YBServer (YbsLogger, DistributedJobLogger)
getLogger r = do
    jlp <- liftIO $ jobLogPath r
    _   <- liftIO $ createDirectoryIfMissing True jlp

    return $ (ybsJobLog jlp, jobLog jlp)


ybsJobLog
    :: FilePath
    -> MachineDescription
    -> Message
    -> YBServer ()
ybsJobLog f md m = liftIO $ jobLog f md m


processJob :: Socket -> JobRequest -> YBServer ()
processJob c r = do
    (ylgr, lgr) <- getLogger r

    ybsReadJobConfig        c r ylgr
        >>= distributeJob     r lgr
        >>= logJobResults   c   ylgr
        >> closeConnection  c

filterMachines
    :: [MachineDescription]
    -> HashMap MachineDescription Hostname
    -> [(MachineDescription, Hostname)]
filterMachines ss xs = toList $ filterWithKey (\k _ -> elem k ss) xs


readConfigFromRepo
    :: GitSource
    -> IO (Either ParseException YbsConfig)
readConfigFromRepo s = withSystemTempDirectory "XXXXXXX." $
    fmap decodeEither' . readFile s ".ybs.yml"
