{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Jobs
    ( JobRequest (..)
    , process
    , CmdResult (..)
    , JobResult (..)
    )
where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Spawn
import Control.Exception hiding (handle)
import Formatting
import Data.Char
import Data.HashMap.Strict
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Data.Text.Lazy hiding (dropWhileEnd, unwords)
import Network.Socket (Socket, send, socketToHandle)
import Prelude hiding (readFile)
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import System.IO (hFlush, hClose, IOMode (..))

import YacBuildServer.Ybs
import YacBuildServer.Git
import YacBuildServer.Logging
import YacBuildServer.Types
import YacBuildServer.Server.Config

readConfigFromRepo
    :: GitSource
    -> FilePath -- working directory
    -> IO (Either ParseException YbsConfig)
readConfigFromRepo s = fmap decodeEither' . readFile s ".ybs.yml"

process :: ConfigServer -> Socket -> JobRequest -> IO ()
process cg c r =
    withSystemTempDirectory "XXXXXXX." proc
  where
    proc w = do
        rcg' <- readJobConfig r w

        jlp <- jobLogPath r
        _   <- createDirectoryIfMissing True jlp

        case rcg' of
            Left e -> do
                _ <- send c (show e)
                jobLog jlp "master" . pack $ show e

            Right rcg -> do
                xs <- distribute cg r rcg (jobLog jlp)
                mapM_ (logResults $ jobLog jlp) xs
                void . send c $ (show xs <> "\n")

        h <- socketToHandle c ReadWriteMode
        _ <- hFlush h
        _ <- hClose h
        return ()

    logResults :: DistributedJobLogger -> Either SomeException JobResult -> IO ()
    logResults l (Left e) = l "master" . pack $ show e
    logResults l (Right (TestResult m cs)) = l m . pack $ show cs

    readJobConfig (CabalTestRequest s) = readConfigFromRepo s
    readJobConfig (MakeCheckRequest s) = readConfigFromRepo s

filterMachines
    :: [MachineDescription]
    -> HashMap MachineDescription Hostname
    -> [(MachineDescription, Hostname)]
filterMachines ss xs = toList $ filterWithKey (\k _ -> elem k ss) xs

distribute
    :: ConfigServer
    -> JobRequest
    -> YbsConfig
    -> DistributedJobLogger
    -> IO [Either SomeException JobResult]
distribute cg r rcg lgr =
    parMapIO
        (\x -> catch
                (Right <$> distribute' r rcg lgr x)
                (return . Left))
        (filterMachines (machineDescription rcg) $ machines cg)

distribute'
    :: JobRequest
    -> YbsConfig
    -> DistributedJobLogger
    -> (MachineDescription, Hostname)
    -> IO JobResult
distribute' r rcg lgr (s, h) = do
    lgr "master" $
        format
            ( "Starting: " % shown
            % " for " % string
            % " at " % string
            % "\n"
            ) r s h

    remoteProcess r rcg s h (lgr s)

data CmdResult = CmdResult
               { cmdResultOut :: String
               , cmdResultErr :: String
               , cmdResultCmd :: Text
               } deriving (Show)

data JobResult = TestResult
               { machine :: MachineDescription
               , commands :: [CmdResult]
               } deriving (Show)

mkCmdResult
    :: (Text -> IO (String, String))
    -> Text
    -> IO CmdResult
mkCmdResult runner cmd = do
    (o, e) <- runner cmd
    return $ CmdResult o e cmd

remoteProcess
    :: JobRequest
    -> YbsConfig
    -> MachineDescription
    -> Hostname
    -> Logger
    -> IO JobResult
remoteProcess (CabalTestRequest s) _ md h _ = do
    d <- (dropWhileEnd isSpace . fst)
        <$> remoteCmd h  "mktemp -dt 'ybs.XXXXXX'"

    c1 <- exe h $ format (
        "cd " % string %
        " && git clone " % text %
        " r && cd r && git checkout " % text
      ) d (uri $ gsUri s) (revOrRef $ gsRevOrRef s)

    c2 <- exe h $
        format (
            "cd " % string %
            " && cabal sandbox init && cabal update" %
            " && PATH=\"/root/.cabal/bin:$PATH\" cabal install" %
            " --only-dependencies -j --enable-tests"
        ) (d </> "r")

    c3 <- exe h $
        format ("cd " % string % " && cabal test") (d </> "r")

    return $ TestResult md [c1, c2, c3]

remoteProcess (MakeCheckRequest s) _ md h _ = do
    d <- (dropWhileEnd isSpace . fst)
        <$> remoteCmd h  "mktemp -dt 'ybs.XXXXXX'"

    c1 <- exe h $ format (
        "cd " % string %
        " && git clone " % text %
        " r && cd r && git checkout " % text
      ) d (uri $ gsUri s) (revOrRef $ gsRevOrRef s)

    c2 <- exe h $
        format (
            "cd " % string %
            " && make check"
        ) (d </> "r")

    return $ TestResult md [c1, c2]

exe :: Hostname -> Text -> IO CmdResult
exe h c = mkCmdResult (remoteCmd h) c

remoteCmd
    :: Hostname
    -> Text
    -> IO (String, String)
remoteCmd h cmd = runCmd Nothing "ssh" [h, unpack cmd]
