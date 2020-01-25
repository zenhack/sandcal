{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module SandCal.ApiTypes
    ( Event(..)
    , Recur(..)

    , SandCalApi
    ) where

import Elm         (ElmType)
import Servant.API

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
    deriving(Show, Read, Eq, Generic, ElmType, ToJSON, FromJSON)

data Recur = Recur
    { until     :: Maybe Int
    , frequency :: ICal.Frequency
    }
    deriving(Show, Read, Eq, Generic, ElmType, ToJSON, FromJSON)

type SandCalApi
    = AllEvents
    :<|> NewEvent
type AllEvents = "all-events.json" :> Get '[JSON] [Event]
type NewEvent = "event" :> "new" :> ReqBody '[JSON] Event :> Post '[JSON] T.Text
