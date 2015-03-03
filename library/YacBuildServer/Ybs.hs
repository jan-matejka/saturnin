{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module YacBuildServer.Ybs
    ( MachineDescription
    , YbsConfig (..)
    , OsUploadConfig (..)
    , decodeFile
    , ParseException (..)
    )
where

import Data.Yaml
import GHC.Generics

type MachineDescription = String

data OsUploadConfig = OsUploadConfig
    { source :: FilePath
    , target :: FilePath
    } deriving (Show, Generic)

instance FromJSON OsUploadConfig

data YbsConfig = YbsConfig
    { machineDescription :: [MachineDescription]
    , os_upload :: Maybe OsUploadConfig
    }
    deriving (Show, Generic)

instance FromJSON YbsConfig
