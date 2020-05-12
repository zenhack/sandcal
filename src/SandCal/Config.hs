module SandCal.Config
    ( Config(..)
    , getConfig
    ) where

import Zhp

import System.Environment (getEnv)

data Config = Config
    { cfgDBPath :: FilePath
    }
    deriving(Show, Read, Eq)

getConfig :: IO Config
getConfig = Config <$> getEnv "DB_PATH"
