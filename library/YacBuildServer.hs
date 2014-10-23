{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer
    ( ybs_main
    )
where

import Network.Socket
import Text.Printf
import Data.Default
import Data.Ini
import Data.Text (pack, unpack)
import Data.HashMap.Strict (empty, fromList, HashMap, toList, filterWithKey)
import System.IO.Error
import System.FilePath.Posix
import System.Process
import System.Exit
import System.IO.Temp
import System.Directory

data ConfigServer = ConfigServer
    { listen_addr   :: Maybe String
    , listen_port   :: Maybe String
    , work_dir      :: Maybe FilePath
    , machines      :: HashMap Machine Hostname
    }

instance Default ConfigServer where
    def = ConfigServer
        { listen_addr = Nothing
        , listen_port = Nothing
        , work_dir    = Nothing
        , machines    = empty
        }

maybeRight :: Either a b -> Maybe b
maybeRight (Left  _) = Nothing
maybeRight (Right x) = Just x

rightOrErr :: Either String Ini -> Ini
rightOrErr (Right x) = x
rightOrErr (Left  x) = error x

readMachinesConfig :: Maybe FilePath -> IO (HashMap Machine Hostname)
readMachinesConfig Nothing = readMachinesConfig $ Just "/etc/ybs/machines"
readMachinesConfig (Just fp) = do
    f <- readFile fp
    return . fromList . fmap ((\(x:(y:_)) -> (x, y)) . words) $ lines f

readConfig :: Maybe FilePath -> IO ConfigServer
readConfig Nothing   = readConfig $ Just "/etc/ybs.ini"
readConfig (Just fp) = do
    ini <- readIniFile fp
    t   <- getTemporaryDirectory
    ms <- readMachinesConfig Nothing

    return $ def
        { listen_addr = gi ini "server" "listen_addr"
        , listen_port = gi ini "server" "listen_port"
        , work_dir    = Just $ combine t "ybs"
        , machines    = ms
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

loop :: ConfigServer -> Socket -> IO ()
loop cg sock = do
    (conn, addr) <- accept sock
    putStrLn $ printf "connected: %s" (show addr)
    catchIOError (handle cg conn) (\_ -> return ())
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
    worker_cmd (Just wdir) "git" ["clone", uri, "repo"]

handle :: ConfigServer -> Socket -> IO ()
handle cg conn = do
    (bytes, _, _) <- recvFrom conn 1024
    let words' = words bytes
        gituri = head words'
        githead = words' !! 1

    withMkTempWorkDir (work_dir cg) "XXXXXXX." $ \wdir -> do
        git_clone wdir gituri
        callCommand $ printf
            "cd %s && git checkout %s" (wdir </> "repo") githead

        repo_cg <- readFile $ wdir </> "repo" </> ".ybs"
        distribute cg gituri githead $ lines repo_cg

distribute :: ConfigServer -> String -> String -> [Machine] -> IO ()
distribute cg gituri githead ms = do
    (mapM_ (distribute' gituri githead) . toList) . filterWithKey (\k _ -> elem k ms) $ machines cg

distribute' :: String -> String -> (Machine, Hostname) -> IO ()
distribute' gu gh (m, h) = do
    putStrLn $ printf "running acceptance testsuite at %s for %s" h m
    callCommand $ printf
        "ssh %s git clone %s /tmp/foo" h gu

    callCommand $ printf
        "ssh %s cd /tmp/foo && git checkout %s" h gh

    callCommand $ printf
        "ssh %s cd /tmp/foo && cabal test" h

type Hostname = String
type Machine  = String
