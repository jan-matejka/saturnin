{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer
    ( ybs_main
    )
where

import Prelude hiding (lookup)
import Control.Concurrent.Spawn
import Network.Socket
import Text.Printf
import Data.Default
import Data.Ini
import Data.Maybe
import Data.Text (pack, unpack)
import Data.HashMap.Strict hiding (map)
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

    _ <- close conn

    withMkTempWorkDir (work_dir cg) "XXXXXXX." $ \wdir -> do
        git_clone wdir gituri
        callCommand $ printf
            "cd %s && git checkout %s" (wdir </> "repo") githead

        repo_cg <- readFile $ wdir </> "repo" </> ".ybs"
        distribute cg gituri githead . fromList . fmap blesmrt $ lines repo_cg
  where
    -- FIXME: Hack to also get list of expected answers for os-release package
    -- since there currently is not any other better way
    blesmrt :: String -> (Machine, String)
    blesmrt xs = (head $ words xs, unwords . tail $ words xs)

distribute :: ConfigServer -> String -> String -> HashMap Machine String -> IO ()
distribute cg gituri githead ms =
    parMapIO_ (distribute' gituri githead) . toList . filterMachines ms $ machines cg


filterMachines :: HashMap Machine String -> HashMap Machine Hostname -> HashMap Machine (Hostname, String)
filterMachines ms xs =
    mapWithKey (\k v1 -> (v1, fromJust $ lookup k ms)) $
    filterWithKey (\k _ -> elem k $ keys ms) xs

remoteCallCommand :: Hostname -> String -> IO ()
remoteCallCommand h cmd = do
    let c = printf "ssh %s '%s'" h cmd
    _ <- printf "[%s]: %s\n" h cmd
    callCommand c

distribute' :: String -> String -> (Machine, (Hostname, String)) -> IO ()
distribute' gu gh (m, (h, s)) = do
    putStrLn $ printf "running acceptance testsuite at %s for %s" h m

    remoteCallCommand h "rm -rf /tmp/foo"

    remoteCallCommand h $ printf "git clone %s /tmp/foo" gu

    remoteCallCommand h $ printf "cd /tmp/foo && git checkout %s" gh

    remoteCallCommand h "uname -a; hostname; cat /etc/os-release"

    remoteCallCommand h $ printf "mkdir -p ~/.config/os-release; echo '%s' > ~/.config/os-release/acceptance.cf" s

    remoteCallCommand h "cd /tmp/foo && cabal sandbox init && cabal update && PATH=\"/root/.cabal/bin:$PATH\" cabal install --only-dependencies -j --enable-tests && cabal test"

type Hostname = String
type Machine  = String
