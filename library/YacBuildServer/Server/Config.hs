{-# LANGUAGE DeriveGeneric #-}
module YacBuildServer.Server.Config
    ( ConfigServer (..)
    , readConfig
    , MachineDescription
    , Hostname
    , YBServerPersistentState (..)
    , readPState
    , writePState
    , JobID (..)
    )
where

import Data.Default
import Data.HashMap.Strict
import Data.Yaml
import GHC.Generics
import System.Directory
import System.FilePath.Posix

type MachineDescription = String
type Hostname = String

data ConfigServer = ConfigServer
    { listen_addr   :: Maybe String
    , listen_port   :: Maybe String
    , machines      :: HashMap MachineDescription Hostname
    , work_dir      :: Maybe FilePath
    } deriving (Show, Generic)

instance FromJSON ConfigServer

instance Default ConfigServer where
    def = ConfigServer
        { listen_addr = Nothing
        , listen_port = Nothing
        , machines    = empty
        , work_dir    = Nothing
        }

readConfig :: IO (Either ParseException ConfigServer)
readConfig = do
    tmp <- getTemporaryDirectory
    cg  <- decodeFileEither "/etc/ybs.yml"

    return $ fmap (defWorkDir tmp) cg
  where
    defWorkDir t (cg @ ConfigServer { work_dir = Nothing }) =
        cg { work_dir = Just $ t </> "ybs" }
    defWorkDir _ cg = cg


data JobID = JobID Int
    deriving (Show, Read, Generic)

instance Enum JobID where
    toEnum x = JobID x
    fromEnum (JobID x) = x

instance FromJSON JobID
instance ToJSON JobID

data YBServerPersistentState = YBServerPersistentState
                             { lastJobID :: JobID }
    deriving (Show, Read, Generic)

instance FromJSON YBServerPersistentState
instance ToJSON YBServerPersistentState

instance Default YBServerPersistentState where
    def = YBServerPersistentState $ JobID 0

readPState :: IO (Either ParseException YBServerPersistentState)
readPState = do
    x <- doesFileExist pstatePath
    if x
    then decodeFileEither pstatePath
    else return $ Right def

writePState :: YBServerPersistentState -> IO ()
writePState = encodeFile pstatePath

pstatePath :: FilePath
pstatePath = "/var/lib/ybs/state"
