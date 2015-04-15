{-# LANGUAGE RankNTypes #-}
module YacBuildServer.Types
    ( MachineDescription
    , Hostname
    , BuildRequest (..)
    , JobRequest (..)
    , TestType (..)
    , GitSource (..)
    , YbsLogger
    , YBServer
    , YBServerState (..)
    , fst3
    )
where

import Control.Monad.State
import Data.Default
import Data.Text.Lazy


import YacBuildServer.Git
import YacBuildServer.Server.Config

data BuildRequest = GitBuildRequest
    { brUri   :: String
    , brHead  :: String
    }

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
    }

instance Default YBServerState where
    def = YBServerState def

type YBServer a = StateT YBServerState IO a


fst3 :: forall t t1 t2.  (t, t1, t2) -> t
fst3 (x, _, _) = x


type YbsLogger = (MachineDescription -> Text -> YBServer ())
