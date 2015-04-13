{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Jobs
    ( JobRequest (..)
    , CmdResult (..)
    , JobResult (..)
    , remoteProcess
    )
where

import Control.Applicative
import Control.Monad
import Formatting
import Data.Char
import Data.List (dropWhileEnd)
import Data.Text.Lazy hiding (dropWhileEnd)
import Prelude hiding (readFile)
import System.FilePath.Posix

import YacBuildServer.Git
import YacBuildServer.Types


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
    -> (MachineDescription, Hostname)
    -> IO JobResult
remoteProcess (CabalTestRequest s) (md, h) = do
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

remoteProcess (MakeCheckRequest s) (md, h) = do
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
