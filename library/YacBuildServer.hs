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
import System.FilePath.Posix
import System.Process
import System.Exit
import System.IO.Temp
import System.Directory

data Config = Config
    { listen_addr   :: Maybe String
    , listen_port   :: Maybe String
    , work_dir      :: Maybe FilePath
    }

instance Default Config where
    def = Config
        { listen_addr = Nothing
        , listen_port = Nothing
        , work_dir    = Nothing
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
    t   <- getTemporaryDirectory

    return $ def
        { listen_addr = gi ini "server" "listen_addr"
        , listen_port = gi ini "server" "listen_port"
        , work_dir    = Just $ combine t "ybs"
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
    loop cg sock

loop :: Config -> Socket -> IO ()
loop cg sock = do
    (conn, addr) <- accept sock
    putStrLn $ printf "connected: %s" (show addr)
    handle cg conn
    loop cg sock


worker_cmd :: Maybe FilePath -> String -> [String] -> IO ()
worker_cmd wdir exe argv = do
    (_, _, _, ph) <- createProcess $ (proc exe argv) { cwd = wdir }
    rc <- waitForProcess ph
    fx rc

  where
    fx (f @ (ExitFailure _)) = error $ printf "%s: %s %s" (show f) (show exe) (show argv)
    fx ExitSuccess = return ()

withMkTempWorkDir ::
       Maybe FilePath
    -> String
    -> (FilePath -> IO a)
    -> IO a
withMkTempWorkDir Nothing _ _ = error "Missing initial workdir"
withMkTempWorkDir (Just wdir) tpl m = do
    catchIOError (createDirectory wdir) mkWdirErrHandler
    withTempDirectory wdir tpl m
  where
    mkWdirErrHandler x
        | isAlreadyExistsError x = return ()
        | otherwise = ioError x

git_clone :: FilePath -> String -> IO ()
git_clone wdir uri = do
    printf "git clone %s into %s" uri wdir
    worker_cmd (Just wdir) "git" ["clone", uri]

handle :: Config -> Socket -> IO ()
handle cg conn = do
    (bytes, _, _) <- recvFrom conn 1024
    let words' = words bytes

    withMkTempWorkDir (work_dir cg) "XXXXXXX" $ \wdir -> do
        git_clone wdir $ head words'
