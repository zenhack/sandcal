{-# LANGUAGE NamedFieldPuns #-}
module Forms.Settings
    ( Settings(..)
    , getForm
    ) where

import Zhp

import Web.Scotty

import Forms.Common

import qualified Data.Time.Zones.All as Tz

data Settings = Settings
    { timeZone :: Tz.TZLabel
    }

getForm :: ActionM Settings
getForm = do
    timeZone <- param "Time Zone" >>= decodeTZLabelOr400
    pure Settings { timeZone }
