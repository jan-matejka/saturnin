{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer
    ( ybs_main
    )
where

import Prelude hiding (lookup)
import Control.Concurrent.Spawn
import Control.Exception (try, catch, SomeException)
import Network.Socket
import Text.Printf
import Data.Default
import Data.Ini
import Data.Text (pack, unpack)
import Data.List (dropWhileEnd)
import Data.Char
import Data.HashMap.Strict hiding (map)
import System.IO.Error
import System.FilePath.Posix
import System.Process
import System.Exit
import System.IO.Temp
import System.Directory

import YacBuildServer.Ybs

data ConfigServer = ConfigServer
    { listen_addr   :: Maybe String
    , listen_port   :: Maybe String
    , work_dir      :: Maybe FilePath
    , machines      :: HashMap Os Hostname
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

readMachinesConfig :: Maybe FilePath -> IO (HashMap Os Hostname)
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
    catch (handle cg conn) ehandler
    loop cg sock
  where
    ehandler :: SomeException -> IO ()
    ehandler ex = printf "Fatal error: %s\n" (show ex)

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

        eex <- try . decodeFile $ wdir </> "repo" </> ".ybs.yml" :: IO (Either ParseException (Maybe YbsConfig))

        case eex of
            Left ex -> do
                _ <- send conn "Can't parse your .ybs.yml\n"
                print ex
            Right ybs_cg -> distribute cg gituri githead ybs_cg $ wdir </> "repo"

    _ <- close conn
    return ()

distribute
    :: ConfigServer
    -> String
    -> String
    -> Maybe YbsConfig
    -> FilePath         -- path to repo
    -> IO ()
distribute _ _ _ Nothing _ = printf "Failed to parse .ybs.yml"
distribute cg gituri githead (Just ybs_cg) repo =
    parMapIO_ (distribute' gituri githead ybs_cg repo) . toList . filterMachines (os ybs_cg) $ machines cg

filterMachines :: [Os] -> HashMap Os Hostname -> HashMap Os Hostname
filterMachines ss xs = filterWithKey (\k _ -> elem k ss) xs

remoteCallCommand :: Hostname -> String -> IO (String, String)
remoteCallCommand h cmd = do
    _ <- printf "[%s]: %s\n" h cmd
    (rc, out, err) <- readProcessWithExitCode "ssh" [h, cmd] ""
    _ <- printf "[%s]: last stdout:\n%s\n" h out
    _ <- printf "[%s]: last stderr:\n%s\n" h err
    case rc of
        ExitSuccess -> return (out, err)
        (ExitFailure {}) -> error $ printf "[%s]: %s: %s" h (show rc) (show cmd)

distribute'
    :: String           -- git uri
    -> String           -- git head
    -> YbsConfig
    -> FilePath         -- repo
    -> (Os, Hostname)
    -> IO ()
distribute' gu gh ybs_cg repo (s, h) = do
    putStrLn $ printf "running acceptance testsuite at %s for %s" h s

    rwdir <- distributeSetup gu gh ybs_cg repo s h
    distributeRunTest h rwdir
    return ()

distributeRunTest
    :: Hostname
    -> FilePath
    -> IO ()
distributeRunTest h d = (remoteCallCommand h $ printf
    "cd %s && cabal test" d) >> return ()

distributeSetup
    :: String           -- git uri
    -> String           -- git head
    -> YbsConfig
    -> FilePath         -- repo
    -> Os
    -> Hostname
    -> IO (FilePath)    -- remote workdir
distributeSetup gu gh ybs_cg repo s h = do
    (out, _) <- remoteCallCommand h "mktemp -dt 'ybs.XXXXXX'"
    distributeOsUpload h repo s $ os_upload ybs_cg

    let rwdir = dropWhileEnd isSpace out

    _ <- remoteCallCommand h $ printf "cd %s && git clone %s r && cd r && git checkout %s"
        rwdir gu gh

    _ <- remoteCallCommand h $ printf "cd %s && cabal sandbox init && cabal update && PATH=\"/root/.cabal/bin:$PATH\" cabal install --only-dependencies -j --enable-tests"
        (rwdir </> "r")

    return $ rwdir </> "r"

distributeOsUpload
    :: Hostname
    -> FilePath
    -> Os
    -> Maybe OsUploadConfig
    -> IO ()
distributeOsUpload _ _ _ Nothing = return ()
distributeOsUpload host repo s (Just ou) =
    callCommand $
    printf "scp -r %s %s:%s"
    (repo </> (source ou) </> s) host (target ou)

type Hostname = String
