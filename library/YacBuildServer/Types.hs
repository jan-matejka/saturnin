{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module YacBuildServer.Types
    ( MachineDescription
    , Hostname
    , BuildRequest (..)
    , JobRequest (..)
    , TestType (..)
    , GitSource (..)
    , YBServer
    , YBServerState (..)
    , fst3
    , TestResult (..)
    , anyEither
    , isPassed
    , JobID (..)
    , defaultYBServerState
    , JobRequestListenerConnectionHandler
    , logError
    , logInfo
    , logToConnection
    , logToConnection'
    )
where

import Control.Applicative  hiding (empty)
import Control.Concurrent.STM
import Control.Monad.State
import Data.Text.Lazy hiding (empty)
import Data.Default
import Data.HashMap.Strict
import Data.Monoid
import Formatting
import Network.Socket

import YacBuildServer.Git
import YacBuildServer.Logging
import YacBuildServer.Server.Config

data BuildRequest = GitBuildRequest
    { brUri   :: String
    , brHead  :: String
    }

type MachinesRegister = HashMap MachineDescription Hostname

-- | JobRequest specifies job to be run. This is what client send to the
-- job server.
data JobRequest = TestRequest
                { testType    :: TestType
                , dataSource :: GitSource
                , testMachines   :: [MachineDescription]
                }
    deriving (Show, Read)

data TestType = CabalTest | MakeCheckTest
    deriving (Show, Read)

data YBServerState = YBServerState
    { ybssConfig :: ConfigServer
    , pState :: TVar YBServerPersistentState
    , freeMachines :: TVar MachinesRegister
    }

-- | Extra function to create default as it needs to run STM
defaultYBServerState :: IO (TVar YBServerState)
defaultYBServerState =
    liftIO . atomically $ do
        x <- newTVar def
        y <- newTVar empty
        z <- newTVar $ YBServerState def x y
        return z

type YBServer a = StateT (TVar YBServerState) IO a

logError :: Text -> YBServer ()
logError = liftIO . logServer . format ("error: " % text % "\n")

logInfo :: Text -> YBServer ()
logInfo = liftIO . logServer . format ("info: " % text % "\n")

type JobRequestListenerConnectionHandler a =
    StateT (Socket, SockAddr) (StateT (TVar YBServerState) IO) a

logToConnection :: Text -> JobRequestListenerConnectionHandler ()
logToConnection x = do
    c <- (fst <$> get)
    liftIO $ logToConnection' c x

logToConnection' :: Socket -> Text -> IO ()
logToConnection' c x =
    void . send c $ unpack x <> "\n"

-- | fst for three-tuple
fst3 :: forall t t1 t2.  (t, t1, t2) -> t
fst3 (x, _, _) = x

data TestResult = Passed | Failed | FailedSetup
    deriving (Show, Read)

isPassed :: TestResult -> Bool
isPassed Passed = True
isPassed _ = False

-- | Returns any thing in Either. Be it Left or Right.
anyEither :: Either a a -> a
anyEither (Left  x) = x
anyEither (Right x) = x
