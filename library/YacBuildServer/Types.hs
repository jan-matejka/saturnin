module YacBuildServer.Types
    ( MachineDescription
    , Hostname
    , BuildRequest (..)
    )
where

type MachineDescription = String
type Hostname = String

data BuildRequest = GitBuildRequest
    { brUri   :: String
    , brHead  :: String
    }
