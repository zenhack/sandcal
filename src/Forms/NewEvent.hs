{-# LANGUAGE NamedFieldPuns #-}
module Forms.NewEvent
    ( NewEvent(..)
    , NewEventTime(..)
    , getForm
    ) where

import Zhp

import Web.Scotty

import Forms.Common

import qualified Data.Map.Strict     as M
import qualified Data.Text.Lazy      as LT
import qualified Data.Time           as Time
import qualified Data.Time.Zones.All as Tz

import qualified DateParsers as DP
import qualified ICal

data NewEvent = NewEvent
    { summary     :: LT.Text
    , description :: LT.Text
    , date        :: Time.Day
    , time        :: NewEventTime
    , repeats     :: Maybe ICal.Frequency
    }

data NewEventTime
    = AllDay
    | StartEnd
        { startTime :: Time.TimeOfDay
        , endTime   :: Time.TimeOfDay
        , timeZone  :: Tz.TZLabel
        }


getForm :: ActionM NewEvent
getForm = do
    summary <- param "Summary"
    description <- param "Description"
    DP.Day date <- param "Date"
    time <- getTime
    repeats <- param "Repeats"
    let repeatsFreq = M.lookup repeats freqNames
    pure NewEvent
        { summary
        , description
        , date
        , time
        , repeats = repeatsFreq
        }

getTime :: ActionM NewEventTime
getTime =
    (AllDay <$ (param "All Day" :: ActionM String))
    `rescue`
    (\_ -> do
        DP.TimeOfDay startTime <- param "Start Time"
        DP.TimeOfDay endTime <- param "End Time"
        tzName <- param "Time Zone"
        tzLabel <- decodeTZLabelOr400 tzName
        pure StartEnd
            { startTime
            , endTime
            , timeZone = tzLabel
            }
    )

freqNames :: M.Map String ICal.Frequency
freqNames =
    [ ICal.Secondly
    , ICal.Minutely
    , ICal.Hourly
    , ICal.Daily
    , ICal.Weekly
    , ICal.Monthly
    , ICal.Yearly
    ]
    & map (\freq -> (show freq, freq))
    & M.fromList
