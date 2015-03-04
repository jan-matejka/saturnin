{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Server
    ( main
    )
where


import Prelude hiding (lookup, log)

import Control.Exception
import Data.Maybe
import Formatting hiding (bind)
import Network.Socket hiding (mkSocket)
import System.Directory
import System.IO.Error
import System.IO

import YacBuildServer.Jobs
import YacBuildServer.Logging
import YacBuildServer.Server.Config

main :: IO ()
main = do
    hClose stdin
    cg <- readConfig
    case cg of
        Left e    -> logError $ format shown e
        Right cg' -> mainWithConf cg'

mainWithConf :: ConfigServer -> IO ()
mainWithConf cg = createWorkDir cg >> listenForRequests cg

createWorkDir :: ConfigServer -> IO ()
createWorkDir cg = modifyIOError ehandle mkWdir
  where
    mkWdir = createDirectoryIfMissing True . fromJust $ work_dir cg
    ehandle e = ioeSetErrorString e
        ((++) "Failed to create working directory" $ ioeGetErrorString e)

listenForRequests :: ConfigServer -> IO ()
listenForRequests cg =
    mkSocket >>= mapM_ acceptConnection . repeat . (,) cg
  where
    mkSocket = do
        addrinfos <- getAddrInfo
            (Just defaultHints {addrFamily = AF_INET})
            (listen_addr cg)
            (listen_port cg)

        let addr = head addrinfos

        sock <- socket (addrFamily addr) Stream defaultProtocol
        _ <- bind sock $ addrAddress addr

        logInfo $ format ("Listening on " % shown) addr
        _ <- listen sock 5

        return sock

acceptConnection :: (ConfigServer, Socket) -> IO ()
acceptConnection (cg, sock) = do
    (conn, addr) <- accept sock
    logInfo $ format ("connected: " % shown) addr
    catch (handleConnection cg conn) ehandle
  where
    ehandle :: SomeException -> IO ()
    ehandle = logError . format ("handleConnection: " % shown)

handleConnection :: ConfigServer -> Socket -> IO ()
handleConnection cg conn = do
    (bytes, _, _) <- recvFrom conn 1024
    process cg conn $ read bytes
