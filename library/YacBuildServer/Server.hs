{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Server
    ( main
    )
where


import Prelude hiding (lookup, log)
import Control.Concurrent.Spawn
import Data.List (dropWhileEnd)
import Data.Char
import Data.HashMap.Strict hiding (map)
import System.IO.Error
import System.Process
import System.Exit


import Control.Exception
import Data.Text.Lazy (Text, pack, unpack)
import Formatting
import Network.Socket hiding (mkSocket)
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Temp

import YacBuildServer.Logging
import YacBuildServer.Server.Config
import YacBuildServer.Types
import YacBuildServer.Ybs

main :: IO ()
main = do
    cg <- readConfig
    case cg of
        Left e    -> logError $ format shown e
        Right cg' -> mainWithConf cg'

mainWithConf :: ConfigServer -> IO ()
mainWithConf cg = listenForRequests cg

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
    let words' = words bytes
        breq   = GitBuildRequest (head words') (words' !! 1)

    withMkTempWorkDir (work_dir cg) "XXXXXXX." $ \wdir -> do
        jlp <- jobLogPath breq
        _ <- createDirectoryIfMissing True jlp
        clone breq wdir (jobLog jlp "master")

        eex <- readYbsConfig $ wdir </> "repo"

        case eex of
            Left e -> do
                _ <- send conn "Can't parse your .ybs.yml\n"
                jobLog jlp "master" . pack $ show e
            Right ybs_cg -> distribute cg breq ybs_cg (wdir </> "repo")
                (jobLog jlp)

    _ <- close conn
    return ()

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

clone
    :: BuildRequest
    -> FilePath
    -> (Text -> IO ())
    -> IO ()
clone (r @ (GitBuildRequest{})) wdir lgr = do
    _ <- runCmd (Just wdir) "git" ["clone", brUri r, "repo"] lgr
    _ <- runCmd (Just (wdir </> "repo")) "git" ["checkout", brHead r] lgr
    return ()

distribute
    :: ConfigServer
    -> BuildRequest
    -> YbsConfig
    -> FilePath         -- path to repo
    -> DistributedJobLogger
    -> IO ()
distribute cg breq ybs_cg repo lgr =
    parMapIO_ (distributor breq ybs_cg repo lgr) . toList . filterMachines (machineDescription ybs_cg) $ machines cg
  where
    distributor a b c d e = catch (distribute' a b c d e) handler
    handler :: SomeException -> IO ()
    handler ex = print ex

distribute'
    :: BuildRequest
    -> YbsConfig
    -> FilePath         -- repo
    -> DistributedJobLogger
    -> (MachineDescription, Hostname)
    -> IO ()
distribute' breq ybs_cg repo lgr (s, h) = do
    lgr "master" $
        format ("running acceptance testsuite at " % string % " for " % string)
            h s

    rwdir <- distributeSetup breq ybs_cg repo s h (lgr s)
    distributeRunTest h rwdir (lgr s)
    return ()

filterMachines :: [MachineDescription] -> HashMap MachineDescription Hostname -> HashMap MachineDescription Hostname
filterMachines ss xs = filterWithKey (\k _ -> elem k ss) xs

remoteCallCommand
    :: Hostname
    -> Logger
    -> Text
    -> IO (String)
remoteCallCommand h lgr cmd = runCmd Nothing "ssh" [h, unpack cmd] lgr


distributeRunTest
    :: Hostname
    -> FilePath
    -> Logger
    -> IO ()
distributeRunTest h d lgr = (remoteCallCommand h lgr $
    format ("cd " % string % " && cabal test") d) >> return ()

distributeSetup
    :: BuildRequest
    -> YbsConfig
    -> FilePath         -- repo
    -> MachineDescription
    -> Hostname
    -> Logger
    -> IO (FilePath)    -- remote workdir
distributeSetup breq ybs_cg repo s h lgr = do
    out  <- remoteCallCommand h lgr "mktemp -dt 'ybs.XXXXXX'"
    distributeOsUpload h repo s $ os_upload ybs_cg

    let rwdir = dropWhileEnd isSpace out

    _ <- remoteCallCommand h lgr $
        format ("cd " % string %
                " && git clone " % string %
                " r && cd r && git checkout " % string)
            rwdir (brUri breq) (brHead breq)

    _ <- remoteCallCommand h lgr $
        format ("cd " % string % " && cabal sandbox init && cabal update && PATH=\"/root/.cabal/bin:$PATH\" cabal install --only-dependencies -j --enable-tests")
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
    callCommand . unpack $
        format ("scp -r " % string % " " % string % ":" % string)
            (repo </> (source ou) </> s) host (target ou)

showCP :: CreateProcess -> Text
showCP c = format
    ("CreateProcess { cmdspec = " % string %
     ", cwd = " % shown % ", env = " % shown % " }")
        (showCmdSpec $ cmdspec c) (cwd c) (env c)

showCmdSpec :: CmdSpec -> String
showCmdSpec (RawCommand exe argv) = unwords
    ["RawCommand", show exe, show argv]
showCmdSpec (ShellCommand cmd) = unwords ["ShellCommand", show cmd]

runCmd
    :: Maybe FilePath       -- cwd
    -> FilePath             -- exe
    -> [String]             -- argv
    -> Logger
    -> IO (String)          -- out
runCmd cwd' exe argv lgr = do
    let cp = (proc exe argv)
            { cwd       = cwd'
            , std_out   = CreatePipe
            , std_err   = CreatePipe
            }
    _ <- lgr $ format ("running: " % text) (showCP cp)
    (_, Just hout, Just herr, ph) <- createProcess cp
    ec <- waitForProcess ph
    out <- hGetContents hout
    err <- hGetContents herr

    _ <- lgr $ format
        ("ec: " % shown %
         "\nstdout:\n" % shown %
         "\nstderr:\n" % shown % "\n")
            ec out err

    fx ec out
  where
    fx (f @ (ExitFailure _)) _ = error . unpack $ format
        (shown % ": " % shown % " " % shown) f exe argv
    fx ExitSuccess out = return out
