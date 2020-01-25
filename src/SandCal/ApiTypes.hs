{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SandCal.ApiTypes
    ( Event(..)
    , Recur(..)
    ) where

import Elminator (ToHType)

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
    , id      :: Maybe T.Text
    }
    deriving(Show, Read, Eq, Generic)
instance ToJSON Event
instance FromJSON Event
instance ToHType Event

data Recur = Recur
    { until     :: Maybe Int
    , frequency :: ICal.Frequency
    }
    deriving(Show, Read, Eq, Generic)
instance ToJSON Recur
instance FromJSON Recur
instance ToHType Recur
