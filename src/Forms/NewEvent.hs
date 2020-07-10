-- | New event/event modification forms.
--
-- TODO: rename this; it used to just be for creating new events, but we are
-- also using this to edit existing ones now.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Forms.NewEvent
    ( NewEvent(..)
    , NewEventTime(..)
    , getForm
    , toVEvent
    , patchVEvent
    ) where

import Zhp

import Web.Scotty

import Forms.Common

import Data.Default             (def)
import Data.Text.Encoding.Error (lenientDecode)

import qualified Data.Aeson              as Aeson
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
    , location    :: LT.Text
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
    location <- param "Location"
    DP.Day date <- param "Date"
    time <- getTime
    repeats <- param "Repeats"
    let repeatsFreq = M.lookup repeats freqNames
    pure NewEvent
        { summary
        , description
        , location
        , date
        , time
        , repeats = repeatsFreq
        }

instance Aeson.ToJSON NewEvent where
    toJSON form = Aeson.toJSON $ M.fromList $ toFields form

toFields :: NewEvent -> [(LT.Text, LT.Text)]
toFields form =
    -- TODO/FIXME: make sure the time format strings are right
    [ ("Summary", summary form)
    , ("Description", description form)
    , ("Location", location form)
    , ("Date", formatTime "%Y-%m-%d" (date form))
    , ("Repeats", case repeats form of
            Nothing   -> "Never"
            Just freq -> fromString $ show freq
      )
    ]
    <> case time form of
        AllDay -> [("All Day", "on")]
        StartEnd {startTime, endTime, timeZone} ->
            [ ("Start Time", formatTime "%H:%M" startTime)
            , ("End Time", formatTime "%H:%M" endTime)
            , ("Time Zone", encodeTZLabel timeZone)
            ]
  where
    formatTime fmt val = fromString $ Time.formatTime Time.defaultTimeLocale fmt val

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

getStartEnd :: NewEvent -> (ICal.DTStart, ICal.DTEnd)
getStartEnd form = case time form of
    AllDay ->
        ( ICal.DTStartDate
            { ICal.dtStartOther = def
            , ICal.dtStartDateValue = ICal.Date (date form)
            }
        , ICal.DTEndDate
            { ICal.dtEndDateValue = ICal.Date (date form) -- TODO/FIXME: should this be exclusive?
            , ICal.dtEndOther = def
            }
        )
    StartEnd { startTime, endTime, timeZone } ->
        let floatingStart = Time.LocalTime
                { Time.localDay = date form
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

-- | Patch an existing vevent based on a modification time and event form.
patchVEvent :: Time.UTCTime -> NewEvent -> ICal.VEvent -> ICal.VEvent
patchVEvent utcNow form old =
    let (start, end) = getStartEnd form in
    old
        { ICal.veSummary = patchTextField (ICal.veSummary old) (summary form)
        , ICal.veDescription = patchTextField (ICal.veDescription old) (description form)
        , ICal.veLocation = patchTextField (ICal.veLocation old) (location form)
        , ICal.veDTStart = Just start
        , ICal.veDTEndDuration = Just $ Left end
        , ICal.veLastMod = Just ICal.LastModified
            { lastModifiedValue = utcNow
            , lastModifiedOther = def
            }
        , ICal.veRRule = makeRRule (repeats form)
        }

patchTextField :: FromTextField a => Maybe a -> LT.Text -> Maybe a
patchTextField old newText =
    case old of
        Nothing   -> fromTextField newText
        Just old' -> Just $ setTextField newText old'

class FromTextField a where
    fromTextField' :: LT.Text -> a
    setTextField :: LT.Text -> a -> a

fromTextField :: FromTextField a => LT.Text -> Maybe a
fromTextField txt
    | LT.null txt = Nothing
    | otherwise = Just (fromTextField' txt)


instance FromTextField ICal.Summary where
    fromTextField' txt = ICal.Summary
        { summaryValue = txt
        , summaryAltRep = def
        , summaryLanguage = def
        , summaryOther = def
        }
    setTextField txt old =
        old { ICal.summaryValue = txt }

instance FromTextField ICal.Location where
    fromTextField' txt = ICal.Location
        { locationValue = txt
        , locationAltRep = def
        , locationLanguage = def
        , locationOther = def
        }
    setTextField txt old =
        old { ICal.locationValue = txt }

instance FromTextField ICal.Description where
    fromTextField' txt = ICal.Description
        { descriptionValue = txt
        , descriptionAltRep = def
        , descriptionLanguage = def
        , descriptionOther = def
        }
    setTextField txt old =
        old { ICal.descriptionValue = txt }

-- | Convert user-supplied event form into an icalendar event.
--
-- Also accepts as arguments the current time, and a uuid to use for the events'
-- unique id.
toVEvent :: Time.UTCTime -> UUID.UUID -> NewEvent -> ICal.VEvent
toVEvent utcNow uuid form =
    let (start, end) = getStartEnd form in
    ICal.VEvent
        { veUID = ICal.UID
            { uidValue = LT.fromStrict $ UUID.toText uuid
            , uidOther = def
            }
        , veDTStamp = ICal.DTStamp
            { dtStampValue = utcNow
            , dtStampOther = def
            }
        , veSummary = fromTextField (summary form)
        , veDTStart = Just start
        , veDTEndDuration = Just $ Left end
        , veCreated = Just ICal.Created
            { createdValue = utcNow
            , createdOther = def
            }
        , veLastMod = Just ICal.LastModified
            { lastModifiedValue = utcNow
            , lastModifiedOther = def
            }
        , veDescription = fromTextField (description form)
        , veRRule = makeRRule (repeats form)
        , veLocation = fromTextField (location form)

        -- Not used for now:
        , veClass = def
        , veGeo = def
        , veOrganizer = def
        , vePriority = def
        , veSeq = def
        , veStatus = def
        , veTransp = def
        , veUrl = def
        , veRecurId = def
        , veAttach = def
        , veAttendee = def
        , veCategories = def
        , veComment = def
        , veContact = def
        , veExDate = def
        , veRStatus  = def
        , veRelated = def
        , veResources = def
        , veRDate = def
        , veAlarms = def
        , veOther = def
        }

makeRRule :: Maybe ICal.Frequency -> S.Set ICal.RRule
makeRRule Nothing = def
makeRRule (Just freq) =
    S.singleton $ ICal.RRule
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

encodeTZLabel :: Tz.TZLabel -> LT.Text
encodeTZLabel =
    Tz.toTZName
    >>> LBS.fromStrict
    >>> LT.decodeUtf8With lenientDecode
