{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module ICal.Types
    ( module Text.ICalendar.Types
    ) where

import Zhp

import Data.Aeson     (FromJSON, ToJSON)
import Database.Selda (SqlType)
import GHC.Generics   (Generic)

import Text.ICalendar.Types

deriving instance Enum Frequency
deriving instance Read  Frequency
deriving instance Bounded Frequency
deriving instance Generic Frequency
instance SqlType Frequency
instance ToJSON Frequency
instance FromJSON Frequency
