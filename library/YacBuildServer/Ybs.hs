{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module YacBuildServer.Ybs
    ( YbsConfig (..)
    , OsUploadConfig (..)
    , ParseException (..)
    , readYbsConfig
    , decodeEither'
    )
where

import Data.Yaml
import GHC.Generics
import System.FilePath.Posix

import YacBuildServer.Types

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

readYbsConfig :: FilePath -> IO (Either ParseException YbsConfig)
readYbsConfig p = decodeFileEither (p </> ".ybs.yml")
