{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer
    ( ybs_main
    )
where

import Prelude hiding (lookup, log)
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
import Data.Time.Clock
import Data.Time.Format
import System.IO.Error
import System.FilePath.Posix
import System.Process
import System.Exit
import System.Locale
import System.IO.Temp
import System.IO
import System.Directory

import YacBuildServer.Ybs

data ConfigServer = ConfigServer
    { listen_addr   :: Maybe String
    , listen_port   :: Maybe String
    , work_dir      :: Maybe FilePath
    , machines      :: HashMap MachineDescription Hostname
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

readMachinesConfig :: Maybe FilePath -> IO (HashMap MachineDescription Hostname)
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

showCP :: CreateProcess -> String
showCP c = printf "CreateProcess { cmdspec = %s, cwd = %s, env = %s }"
    (showCmdSpec $ cmdspec c) (show $ cwd c) (show $ env c)

showCmdSpec :: CmdSpec -> String
showCmdSpec (RawCommand exe argv) = printf "RawCommand %s %s" (show exe) (show argv)
showCmdSpec (ShellCommand cmd) = printf "ShellCommand %s" (show cmd)

runCmd
    :: Maybe FilePath       -- cwd
    -> FilePath             -- exe
    -> [String]             -- argv
    -> (String -> IO ())    -- logger
    -> IO (String)          -- out
runCmd cwd' exe argv lgr = do
    let cp = (proc exe argv)
            { cwd       = cwd'
            , std_out   = CreatePipe
            , std_err   = CreatePipe
            }
    _ <- lgr $ printf "running: %s\n" (showCP cp)
    (_, Just hout, Just herr, ph) <- createProcess cp
    ec <- waitForProcess ph
    out <- hGetContents hout
    err <- hGetContents herr

    _ <- lgr $ printf "ec: %s\nstdout:\n%s\nstderr:\n%s\n"
            (show ec) (show out) (show err)

    fx ec out
  where
    fx (f @ (ExitFailure _)) _ = error $ printf "%s: %s %s" (show f) (show exe) (show argv)
    fx ExitSuccess out = return out

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

data BuildRequest = GitBuildRequest
    { brUri   :: String
    , brHead  :: String
    }

dirname :: BuildRequest -> String
dirname (GitBuildRequest u h) = printf "%s-%s" (san u) (san h)
  where
    san :: String -> String
    san = fmap (\x -> if isAlphaNum x then x else '_')

clone
    :: BuildRequest
    -> FilePath
    -> (String -> IO ())
    -> IO ()
clone (r @ (GitBuildRequest{})) wdir lgr = do
    _ <- runCmd (Just wdir) "git" ["clone", brUri r, "repo"] lgr
    _ <- runCmd (Just (wdir </> "repo")) "git" ["checkout", brHead r] lgr
    return ()

data LoggingNamespace = LoggingNamespace
    { br    :: BuildRequest
    , time  :: UTCTime
    , dir   :: FilePath
    }

log :: LoggingNamespace -> String -> String -> IO ()
log ln m msg = appendFile ((dir ln) </> (printf "%s.txt" m)) msg

mkLoggingNamespace :: BuildRequest -> IO LoggingNamespace
mkLoggingNamespace br = do
    t <- getCurrentTime
    let lns = LoggingNamespace br t $ "/var/lib/ybs/logs" </> (dirname br) </> (formatTime defaultTimeLocale "%F.%T.%Z" t)
    _ <- createDirectoryIfMissing True $ dir lns
    return lns

handle :: ConfigServer -> Socket -> IO ()
handle cg conn = do
    (bytes, _, _) <- recvFrom conn 1024
    let words' = words bytes
        breq   = GitBuildRequest (head words') (words' !! 1)

    withMkTempWorkDir (work_dir cg) "XXXXXXX." $ \wdir -> do
        lns <- mkLoggingNamespace breq
        clone breq wdir (log lns "master")

        eex <- try . decodeFile $ wdir </> "repo" </> ".ybs.yml" :: IO (Either ParseException (Maybe YbsConfig))

        case eex of
            Left ex -> do
                _ <- send conn "Can't parse your .ybs.yml\n"
                _ <- log lns "master" $ show ex
                return ()
            Right ybs_cg -> distribute cg breq ybs_cg (wdir </> "repo") (log lns)

    _ <- close conn
    return ()

distribute
    :: ConfigServer
    -> BuildRequest
    -> Maybe YbsConfig
    -> FilePath         -- path to repo
    -> (FilePath -> String -> IO ())
    -> IO ()
distribute _ _ Nothing _ lgr = lgr "master" "Failed to parse .ybs.yml"
distribute cg breq (Just ybs_cg) repo lgr =
    parMapIO_ (distributor breq ybs_cg repo lgr) . toList . filterMachines (machineDescription ybs_cg) $ machines cg
  where
    distributor a b c d e = catch (distribute' a b c d e) handler
    handler :: SomeException -> IO ()
    handler ex = print ex

filterMachines :: [MachineDescription] -> HashMap MachineDescription Hostname -> HashMap MachineDescription Hostname
filterMachines ss xs = filterWithKey (\k _ -> elem k ss) xs

remoteCallCommand
    :: Hostname
    -> (String -> IO ())
    -> String
    -> IO (String)
remoteCallCommand h lgr cmd = runCmd Nothing "ssh" [h, cmd] lgr

distribute'
    :: BuildRequest
    -> YbsConfig
    -> FilePath         -- repo
    -> (FilePath -> String -> IO ())
    -> (MachineDescription, Hostname)
    -> IO ()
distribute' breq ybs_cg repo lgr (s, h) = do
    lgr "master" $ printf "running acceptance testsuite at %s for %s" h s

    rwdir <- distributeSetup breq ybs_cg repo s h (lgr s)
    distributeRunTest h rwdir (lgr s)
    return ()

distributeRunTest
    :: Hostname
    -> FilePath
    -> (String -> IO ())
    -> IO ()
distributeRunTest h d lgr = (remoteCallCommand h lgr $ printf
    "cd %s && cabal test" d) >> return ()

distributeSetup
    :: BuildRequest
    -> YbsConfig
    -> FilePath         -- repo
    -> MachineDescription
    -> Hostname
    -> (String -> IO ())
    -> IO (FilePath)    -- remote workdir
distributeSetup breq ybs_cg repo s h lgr = do
    out  <- remoteCallCommand h lgr "mktemp -dt 'ybs.XXXXXX'"
    distributeOsUpload h repo s $ os_upload ybs_cg

    let rwdir = dropWhileEnd isSpace out

    _ <- remoteCallCommand h lgr $ printf "cd %s && git clone %s r && cd r && git checkout %s"
        rwdir (brUri breq) (brHead breq)

    _ <- remoteCallCommand h lgr $ printf "cd %s && cabal sandbox init && cabal update && PATH=\"/root/.cabal/bin:$PATH\" cabal install --only-dependencies -j --enable-tests"
        (rwdir </> "r")

    return $ rwdir </> "r"

distributeOsUpload
    :: Hostname
    -> FilePath
    -> MachineDescription
    -> Maybe OsUploadConfig
    -> IO ()
distributeOsUpload _ _ _ Nothing = return ()
distributeOsUpload host repo s (Just ou) =
    callCommand $
    printf "scp -r %s %s:%s"
    (repo </> (source ou) </> s) host (target ou)

type Hostname = String
