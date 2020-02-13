{-# LANGUAGE NamedFieldPuns #-}
module ICal.Convert
    ( eventToDB
    ) where

import Zhp

import qualified Data.Set       as Set
import qualified Database.Selda as Selda

import qualified SandCal.DB as DB

import Text.ICalendar.Types

eventToDB :: VEvent -> (DB.Event, [DB.Recur])
eventToDB VEvent{veSummary, veDTStart, veRRule} =
    ( DB.Event
        { DB.evId = Selda.def
        , DB.evSummary = case veSummary of
            Nothing                    -> ""
            Just Summary{summaryValue} -> summaryValue
        , DB.evDTStart = case veDTStart of
            DTStartDateTime{dtStartDateTimeValue = dt} -> dateTimeToUnix dt
            DTStartDate{dtStartDateValue = d}          -> dateToUnix d
        }
    , [ DB.Recur
            { DB.rEventId = Selda.def
            , DB.rFrequency = recurFreq
            , DB.rUntil = Nothing -- FIXME: put something here.
            }
      | RRule
            { rRuleValue = Recur
                { recurFreq
                }
            } <- Set.toList veRRule
      ]
    )

dateTimeToUnix :: DateTime -> Int
dateTimeToUnix _ = error "TODO"

dateToUnix :: Date -> Int
dateToUnix Date{dateValue} = error "TODO"
