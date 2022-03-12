module Config
  ( Config (..),
    getConfig,
  )
where

import System.Environment (getEnv)
import Zhp

data Config = Config
  { cfgDBPath :: FilePath
  }
  deriving (Show, Read, Eq)

getConfig :: IO Config
getConfig = Config <$> getEnv "DB_PATH"
