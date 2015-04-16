{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Server
    ( runYBServer
    )
where

import Prelude hiding (lookup, log, readFile)

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.State
import Data.Either.Combinators
import Data.Maybe
import Formatting (format, shown, text, (%))
import Network.Socket
import System.Directory
import System.IO hiding (readFile)

import YacBuildServer.Server.Config
import YacBuildServer.Server.Connection
import YacBuildServer.Types

runYBServer :: IO ()
runYBServer = defaultYBServerState >>= evalStateT serve
  where
    serve :: YBServer ()
    serve =
        ybsCloseStdin
        >> ybsReadConfig
        >>= ybsReadState
        >>= ybsCreateWorkdir
        >>= ybsListen
        >>= ybsAccept

ybsCloseStdin :: YBServer ()
ybsCloseStdin = liftIO $ hClose stdin

ybsReadConfig :: YBServer (Maybe ConfigServer)
ybsReadConfig = do
    x <- liftIO readConfig
    whenLeft  x $ logError . format shown
    whenRight x $ \z -> get >>= \y -> put y { ybssConfig = z }
    return $ rightToMaybe x

ybsReadState
    :: Maybe ConfigServer
    -> YBServer (Maybe ConfigServer)
ybsReadState Nothing = return Nothing
ybsReadState (Just cg) = do
    y <- liftIO readPState
    whenLeft  y $ logError . format shown
    whenRight y $ \z -> do
        s <- liftIO . atomically $ newTVar z
        get >>= \t -> put t { pState = s }
    return $ const cg <$> rightToMaybe y

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
ybsAccept (Just x) = mapM_ ((handleConnection =<<) . (liftIO . accept)) $ repeat x
ybsAccept Nothing = return ()
