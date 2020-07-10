{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Forms.NewEvent
    ( NewEvent(..)
    , NewEventTime(..)
    , getForm
    , toVEvent
    ) where

import Zhp

import Web.Scotty

import Forms.Common

import Data.Default             (def)
import Data.Text.Encoding.Error (lenientDecode)

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Time               as Time
import qualified Data.Time.Zones.All     as Tz
import qualified Data.UUID               as UUID

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

toVEvent :: Time.UTCTime -> UUID.UUID -> NewEvent -> ICal.VEvent
toVEvent utcNow uuid Forms.NewEvent.NewEvent{ summary, description, date, time, repeats } =
    let (start, end) = case time of
            Forms.NewEvent.AllDay ->
                ( ICal.DTStartDate
                    { ICal.dtStartOther = def
                    , ICal.dtStartDateValue = ICal.Date date
                    }
                , ICal.DTEndDate
                    { ICal.dtEndDateValue = ICal.Date date -- TODO/FIXME: should this be exclusive?
                    , ICal.dtEndOther = def
                    }
                )
            Forms.NewEvent.StartEnd { startTime, endTime, timeZone } ->
                let floatingStart = Time.LocalTime
                        { Time.localDay = date
                        , Time.localTimeOfDay = startTime
                        }
                    floatingEnd = floatingStart
                        { Time.localTimeOfDay = endTime
                        }
                    dtStart = ICal.ZonedDateTime
                        { ICal.dateTimeFloating = floatingStart
                        , ICal.dateTimeZone = encodeTZLabel timeZone
                        }
                    dtEnd = dtStart { ICal.dateTimeFloating = floatingEnd }
                in
                ( ICal.DTStartDateTime
                    { ICal.dtStartOther = def
                    , ICal.dtStartDateTimeValue = dtStart
                    }
                , ICal.DTEndDateTime
                    { ICal.dtEndDateTimeValue = dtEnd
                    , ICal.dtEndOther = def
                    }
                )
    in
    ICal.VEvent
        { ICal.veUID = ICal.UID
            { ICal.uidValue = LT.fromStrict $ UUID.toText uuid
            , ICal.uidOther = def
            }
        , ICal.veDTStamp = ICal.DTStamp
            { ICal.dtStampValue = utcNow
            , ICal.dtStampOther = def
            }
        , ICal.veSummary = Just ICal.Summary
            { ICal.summaryValue = summary
            , ICal.summaryAltRep = def
            , ICal.summaryLanguage = def
            , ICal.summaryOther = def
            }
        , ICal.veDTStart = Just start
        , ICal.veDTEndDuration = Just $ Left end
        -- TODO: fill these in with the current time:
        , ICal.veCreated = Nothing
        , ICal.veLastMod = Nothing

        , ICal.veDescription =
            case description of
                "" ->
                    Nothing

                _ ->
                    Just ICal.Description
                        { ICal.descriptionValue = description
                        , ICal.descriptionAltRep = def
                        , ICal.descriptionLanguage = def
                        , ICal.descriptionOther = def
                        }

        -- Not used for now:
        , ICal.veClass = def
        , ICal.veGeo = def
        , ICal.veLocation = def
        , ICal.veOrganizer = def
        , ICal.vePriority = def
        , ICal.veSeq = def
        , ICal.veStatus = def
        , ICal.veTransp = def
        , ICal.veUrl = def
        , ICal.veRecurId = def
        , ICal.veRRule =
            case repeats of
                Nothing -> def
                Just freq -> S.singleton $ ICal.RRule
                    { rRuleOther = def
                    , rRuleValue = ICal.Recur
                        { recurFreq = freq
                        , recurUntilCount = def
                        , recurInterval = 1
                        , recurBySecond = def
                        , recurByMinute = def
                        , recurByHour = def
                        , recurByDay = def
                        , recurByMonthDay = def
                        , recurByYearDay = def
                        , recurByWeekNo = def
                        , recurByMonth = def
                        , recurBySetPos = def
                        , recurWkSt = ICal.Sunday
                          -- ^ Does this matter? Not for our current usage,
                          -- but we should research what it means.
                        }
                    }
        , ICal.veAttach = def
        , ICal.veAttendee = def
        , ICal.veCategories = def
        , ICal.veComment = def
        , ICal.veContact = def
        , ICal.veExDate = def
        , ICal.veRStatus  = def
        , ICal.veRelated = def
        , ICal.veResources = def
        , ICal.veRDate = def
        , ICal.veAlarms = def
        , ICal.veOther = def
        }

encodeTZLabel :: Tz.TZLabel -> LT.Text
encodeTZLabel =
    Tz.toTZName
    >>> LBS.fromStrict
    >>> LT.decodeUtf8With lenientDecode
