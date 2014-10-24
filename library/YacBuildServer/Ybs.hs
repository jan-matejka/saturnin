{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module YacBuildServer.Ybs
    ( Os
    , YbsConfig (..)
    , OsUploadConfig (..)
    , decodeFile
    )
where

import Data.Yaml
import GHC.Generics

type Os = String

data OsUploadConfig = OsUploadConfig
    { source :: FilePath
    , target :: FilePath
    } deriving (Show, Generic)

instance FromJSON OsUploadConfig

data YbsConfig = YbsConfig
    { os    :: [Os]
    , os_upload :: Maybe OsUploadConfig
    }
    deriving (Show, Generic)

instance FromJSON YbsConfig
