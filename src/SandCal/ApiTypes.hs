{-# LANGUAGE DeriveGeneric #-}
module SandCal.ApiTypes
    ( Event(..)
    ) where

import Zhp

import qualified Data.Text as T

import Data.Aeson
import GHC.Generics (Generic)

data Event = Event
    { summary :: T.Text
    , start   :: !Int
    , end     :: !Int
    }
    deriving(Show, Read, Eq, Generic)
instance ToJSON Event
instance FromJSON Event
