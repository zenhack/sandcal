{-# LANGUAGE DeriveGeneric #-}
module SandCal.ApiTypes
    ( Event(..)
    , Recur(..)
    ) where

import Zhp

import qualified Data.Text as T

import Data.Aeson
import GHC.Generics (Generic)

import qualified ICal.Types as ICal

data Event = Event
    { summary :: T.Text
    , start   :: !Int
    , end     :: !Int
    , recurs  :: [Recur]
    }
    deriving(Show, Read, Eq, Generic)
instance ToJSON Event
instance FromJSON Event

data Recur = Recur
    { until     :: Maybe Int
    , frequency :: ICal.Frequency
    }
    deriving(Show, Read, Eq, Generic)
instance ToJSON Recur
instance FromJSON Recur
