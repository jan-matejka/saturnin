{-# LANGUAGE OverloadedStrings #-}
module Saturnin.Server
    ( runYBServer
    )
where

import Prelude hiding (lookup, log, readFile)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.State
import Data.Default
import Data.Either.Combinators
import Data.Maybe
import Formatting (format, shown, text, (%))
import Network.Socket
import System.Directory
import System.IO hiding (readFile)

import Saturnin.Server.Config
import Saturnin.Server.Connection
import Saturnin.Types

runYBServer :: IO ()
runYBServer = atomically (newTVar def) >>= evalStateT serve
  where
    serve :: YBServer ()
    serve =
        ybsCloseStdin
        >> openLogFile
        >> ybsReadConfig
        >>= ybsReadState
        >>= registerMachines
        >>= ybsCreateWorkdir
        >>= ybsListen
        >>= ybsAccept

ybsCloseStdin :: YBServer ()
ybsCloseStdin = liftIO $ hClose stdin

openLogFile :: YBServer ()
openLogFile = do
    ts <- get
    h <- liftIO $ openFile "/var/log/ybs.log" AppendMode
    liftIO . atomically $ do
        s <- readTVar ts
        writeTVar ts $ s { logHandle = h }

ybsReadConfig :: YBServer (Maybe ConfigServer)
ybsReadConfig = do
    x <- liftIO readConfig
    whenLeft  x $ logError . format shown
    whenRight x $ \z -> get >>= \ts ->
        liftIO . atomically $ do
            s <- readTVar ts
            writeTVar ts $ s { ybssConfig = z }
    return $ rightToMaybe x

ybsReadState
    :: Maybe ConfigServer
    -> YBServer (Maybe ConfigServer)
ybsReadState Nothing = return Nothing
ybsReadState (Just cg) = do
    eps <- liftIO readPState
    whenLeft  eps $ logError . format shown
    ts <- get
    whenRight eps $ \ps -> liftIO . atomically $ do
        s <- readTVar ts
        writeTVar ts $ s { pState = ps }
    return $ const cg <$> rightToMaybe eps

registerMachines
    :: Maybe ConfigServer
    -> YBServer (Maybe ConfigServer)
registerMachines (Just cg) = do
    ts <- get
    liftIO . atomically $ do
        s <- readTVar ts
        writeTVar ts $ s { freeMachines = machines cg }
    return (Just cg)
registerMachines Nothing = return Nothing

ybsCreateWorkdir :: Maybe ConfigServer -> YBServer (Maybe ConfigServer)
ybsCreateWorkdir (Just cg) = do
    catch
        (liftIO . createDirectoryIfMissing True . fromJust $ work_dir cg)
        (logError . format (text % shown) p :: SomeException -> YBServer ())
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

    logInfo $ format ("Listening on " % shown) addr
    _ <- liftIO $ listen sock 5

    return $ Just sock
ybsListen Nothing = return Nothing

ybsAccept :: Maybe Socket -> YBServer ()
ybsAccept (Just x) = do
    ts <- get
    forever $ do
        c <- liftIO $ accept x
        void . liftIO . forkIO $ evalStateT (handleConnection c) ts
ybsAccept Nothing = return ()
