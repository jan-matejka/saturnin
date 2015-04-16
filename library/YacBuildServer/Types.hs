{-# LANGUAGE RankNTypes #-}
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
    )
where

import Control.Concurrent.STM
import Control.Monad.State
import Data.Default

import YacBuildServer.Git
import YacBuildServer.Server.Config

data BuildRequest = GitBuildRequest
    { brUri   :: String
    , brHead  :: String
    }

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
    }

-- | Extra function to create default as it needs to run STM
defaultYBServerState :: IO YBServerState
defaultYBServerState = do
    s <- liftIO . atomically $ newTVar def
    return $ YBServerState def s

type YBServer a = StateT YBServerState IO a

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
