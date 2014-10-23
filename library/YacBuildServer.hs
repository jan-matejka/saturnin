{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer
    ( ybs_main
    )
where

import Network.Socket
import Text.Printf
import Data.Default
import Data.Ini
import Data.Text hiding (head, words)
import System.IO.Error

data Config = Config
    { listen_addr   :: Maybe String
    , listen_port   :: Maybe String
    }

instance Default Config where
    def = Config
        { listen_addr = Nothing
        , listen_port = Nothing
        }

maybeRight :: Either a b -> Maybe b
maybeRight (Left  _) = Nothing
maybeRight (Right x) = Just x

rightOrErr :: Either String Ini -> Ini
rightOrErr (Right x) = x
rightOrErr (Left  x) = error x

readConfig :: Maybe FilePath -> IO Config
readConfig Nothing   = readConfig $ Just "/etc/ybs.ini"
readConfig (Just fp) = do
    ini <- readIniFile fp

    return $ def
        { listen_addr = gi ini "server" "listen_addr"
        , listen_port = gi ini "server" "listen_port"
        }
  where
    gi :: Either String Ini -> String -> String -> Maybe String
    gi ini sec key = fmap unpack . maybeRight . lookupValue (pack sec) (pack key) $ rightOrErr ini

ybs_main :: IO ()
ybs_main = do
    cg <- catchIOError (readConfig Nothing) (\_ -> return def)

    addrinfos <- getAddrInfo
        (Just defaultHints {addrFamily = AF_INET})
        (listen_addr cg)
        (listen_port cg)

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
    (bytes, _, _) <- recvFrom conn 1024
    let words' = words bytes
    return ()
