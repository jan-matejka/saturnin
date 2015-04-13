module YacBuildServer.Types
    ( MachineDescription
    , Hostname
    , BuildRequest (..)
    , JobRequest (..)
    )
where

import YacBuildServer.Git

type MachineDescription = String
type Hostname = String

data BuildRequest = GitBuildRequest
    { brUri   :: String
    , brHead  :: String
    }

data JobRequest = CabalTestRequest GitSource
                | MakeCheckRequest GitSource
    deriving (Show, Read)
