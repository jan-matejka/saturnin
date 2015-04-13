{-# LANGUAGE RankNTypes #-}
module YacBuildServer.Types
    ( MachineDescription
    , Hostname
    , BuildRequest (..)
    , JobRequest (..)
    , gitSource
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

data JobRequest = CabalTestRequest GitSource
                | MakeCheckRequest GitSource
    deriving (Show, Read)

gitSource :: JobRequest -> GitSource
gitSource (CabalTestRequest s) = s
gitSource (MakeCheckRequest s) = s

data YBServerState = YBServerState
    { ybssConfig :: ConfigServer
    }

instance Default YBServerState where
    def = YBServerState def

type YBServer a = StateT YBServerState IO a


fst3 :: forall t t1 t2.  (t, t1, t2) -> t
fst3 (x, _, _) = x


type YbsLogger = (MachineDescription -> Text -> YBServer ())
