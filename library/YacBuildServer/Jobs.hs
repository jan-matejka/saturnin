{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Jobs
    ( JobRequest (..)
    , CmdResult (..)
    , JobResult (..)
    , Job (..)
    , RemoteJob (..)
    , mkRemoteJob
    , runRemoteJob
    , RemoteJobRunnerState (..)
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Formatting
import Data.Either.Combinators
import Data.Text.Lazy hiding (dropWhileEnd)
import Prelude hiding (readFile)
import System.Exit
import System.IO
import System.Process

import YacBuildServer.Git
import YacBuildServer.Logging
import YacBuildServer.Types

-- | Job fully describes a job to be run. This is what JobRequest is
-- translated into for internal representation. This holds data that are
-- needed to actually run the job - that is data derived from the
-- JobRequest, eg.: [Hostname] instead of [MachineDescription]
data Job = Job
         { remoteJobs       :: [RemoteJob]
         , request          :: JobRequest
         , jobID            :: JobID
         } deriving (Show)

-- | RemoteJob describes a job to be run on one machine. RemoteJob
-- is always part of Job.
data RemoteJob = TestJob
         { rJobTestType     :: TestType
         , rJobDataSource   :: GitSource
         , jobMachine       :: MachineDescription
         , jobHost          :: Hostname
         } deriving (Show)

mkRemoteJob
    :: JobRequest
    -> MachineDescription
    -> Hostname
    -> RemoteJob
mkRemoteJob x = TestJob (testType x) (dataSource x)

getJob :: RemoteJobRunner RemoteJob
getJob = rJob <$> get

data RemoteJobRunnerState = RemoteJobRunnerState
                          { rJob :: RemoteJob
                          , rJobLogger :: Logger
                          , cLogger :: Logger
                          }
type RemoteJobRunner a = StateT RemoteJobRunnerState IO a

runRemoteJob
    :: RemoteJobRunnerState
    -> IO JobResult
runRemoteJob x = evalStateT run x
  where
    run :: RemoteJobRunner JobResult
    run =   mkWorkDir
        >>= checkoutDataSource
        >>= prepareEnvironment
        >>= endTestSetup
        >>= runTest
        >>= mkJobResult

mkJobResult
    :: Either TestResult a
    -> RemoteJobRunner JobResult
mkJobResult x = do
    j <- getJob
    return $ JobResult (jobMachine j) (getResult x)
  where
    getResult (Left  y) = y
    getResult (Right _) = Passed

rJobLogToConnection :: Text -> RemoteJobRunner ()
rJobLogToConnection x = do
    c <- cLogger <$> get
    liftIO $ c x

logRemoteJob :: Text -> RemoteJobRunner ()
logRemoteJob x = do
    rJobLogToConnection x
    rl <- rJobLogger <$> get
    liftIO $ rl x

-- | Run a command on the remote machine.
-- Log the CmdResult to the job's log file and client connection.
-- And return Right CmdResult on success or Left Failed on error.
exe :: Text -> RemoteJobRunner (Either TestResult CmdResult)
exe x = do
    j <- getJob
    r <- liftIO . remoteCmd x $ jobHost j
    logRemoteJob . pack . show $ anyEither r
    return $ mapLeft (const Failed) r

-- | Returns `Left FailedSetup` iff `Left _`
-- otherwise `Right _`
endTestSetup
    :: Either TestResult WorkDir
    -> RemoteJobRunner (Either TestResult WorkDir)
endTestSetup = return . mapLeft (const FailedSetup)

type WorkDir = Text

-- | Returns `Right WorkDir` or `Left Failed`
mkWorkDir :: RemoteJobRunner (Either TestResult WorkDir)
mkWorkDir = (<$>) (strip . cmdResultOut) <$> (exe "mktemp -dt 'ybs.XXXXXX'")

-- | Returns `Right WorkDir` of the data source or `Left Failed`
checkoutDataSource
    :: Either TestResult WorkDir
    -> RemoteJobRunner (Either TestResult WorkDir)
checkoutDataSource (Right p) = do
    j <- getJob
    (<$>) (const repo) <$> (exe . cmd $ rJobDataSource j)
  where
    cmd s = format (
        " git clone         " % text % " " % text %
        " && cd             " % text %
        " && git checkout   " % text
        )
        (uri $ gsUri s) repo
        repo
        (revOrRef $ gsRevOrRef s)

    repo = intercalate "/" [p, "repo"]
checkoutDataSource (Left x) = return $ Left x

prepareEnvironment
    :: Either TestResult WorkDir
    -> RemoteJobRunner (Either TestResult WorkDir)
prepareEnvironment (Right p) = do
    j <- getJob
    (<$>) (const p) <$> (exe' $ rJobTestType j)
  where
    exe' CabalTest = exe $ format (
        "cd " % text %
        " && cabal sandbox init && cabal update" %
        " && PATH=\"/root/.cabal/bin:$PATH\" cabal install" %
        " --only-dependencies -j --enable-tests"
        ) p
    exe' MakeCheckTest = return $ Right undefined
prepareEnvironment (Left x) = return $ Left x

runTest
    :: Either TestResult WorkDir
    -> RemoteJobRunner (Either TestResult CmdResult)
runTest (Right p) = do
    j <- getJob
    exe . cmd $ rJobTestType j
  where
    cmd CabalTest = format ("cd " % text % " && cabal test") p
    cmd MakeCheckTest = format ("cd " % text % " && make check") p
runTest (Left x) = return $ Left x

data CmdResult = CmdResult
               { cmdResultOut   :: Text
               , cmdResultErr   :: Text
               , cmdResultCmd   :: Text
               , cmdResultCode  :: ExitCode
               } deriving (Show)

data JobResult = JobResult
               { machine :: MachineDescription
               , result :: TestResult
               } deriving (Show)

-- | run command Text on remote Hostname and return `Left CmdResult` on
-- error and `Right CmdResult` on success.
remoteCmd
    :: Text
    -> Hostname
    -> IO (Either CmdResult CmdResult)
remoteCmd cmd h = do
    (_, Just hout, Just herr, ph) <- createProcess cp
    ec  <- waitForProcess ph
    out <- pack <$> hGetContents hout
    err <- pack <$> hGetContents herr

    return . either' ec $ CmdResult out err cmd ec
  where
    cp = (proc "ssh" [h, unpack cmd])
        { cwd       = Just "/"
        , std_out   = CreatePipe
        , std_err   = CreatePipe
        }

    either' ExitSuccess     x = Right x
    either' (ExitFailure _) x = Left  x
