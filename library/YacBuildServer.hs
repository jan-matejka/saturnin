{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer
    ( ybs_main
    )
where

import Network.Socket
import Text.Printf

ybs_main :: IO ()
ybs_main = do
    let port = "7777"
        addr = "0.0.0.0"

    addrinfos <- getAddrInfo
        (Just defaultHints {addrFamily = AF_INET})
        (Just addr)
        (Just port)

    putStrLn "Availble addrinfos:"
    mapM_ print addrinfos

    let ainf = head addrinfos

    sock <- socket (addrFamily ainf) Stream defaultProtocol
    _ <- bind sock $ addrAddress ainf
    _ <- listen sock 5
    loop sock

loop :: Socket -> IO ()
loop sock = do
    (conn, addr) <- accept sock
    handle conn addr
    loop sock

handle :: Socket -> SockAddr -> IO ()
handle conn addr = do
    putStrLn $ printf "connected: %s" (show addr)
