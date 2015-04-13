{-# LANGUAGE DeriveGeneric #-}
module YacBuildServer.Server.Config
    ( ConfigServer (..)
    , readConfig
    , MachineDescription
    , Hostname
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
