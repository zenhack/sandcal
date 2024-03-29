{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | New event/event modification forms.
--
-- TODO: rename this; it used to just be for creating new events, but we are
-- also using this to edit existing ones now.
module Forms.NewEvent
  ( NewEvent (..),
    NewEventTime (..),
    fromVEvent,
    toVEvent,
    patchVEvent,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import qualified Data.Set as S
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Forms.Common
import GHC.Generics (Generic)
import qualified Util.ICal as ICal
import qualified Util.Scotty.DateParsers as DP
import qualified Util.TZ as TZ
import Zhp

-- TODO: can't represent events that cross a day boundary.
-- TODO: this is no longer just used for "new" events; change the name.
data NewEvent = NewEvent
  { summary :: LT.Text,
    description :: LT.Text,
    location :: LT.Text,
    date :: Time.Day,
    time :: NewEventTime,
    repeats :: [RepeatRule]
  }
  deriving (Show, Generic)

instance Aeson.ToJSON NewEvent

instance Aeson.FromJSON NewEvent

data RepeatRule = RepeatRule
  { frequency :: ICal.Frequency,
    interval :: !Int
  }
  deriving (Show, Generic)

instance Aeson.ToJSON RepeatRule

instance Aeson.FromJSON RepeatRule

data NewEventTime
  = AllDay
  | StartEnd
      { startTime :: DP.TimeOfDay,
        endTime :: DP.TimeOfDay,
        timeZone :: TZ.TZLabel
      }
  deriving (Show, Generic)

instance Aeson.ToJSON NewEventTime

instance Aeson.FromJSON NewEventTime

getStartEnd :: NewEvent -> (ICal.DTStart, ICal.DTEnd)
getStartEnd form = case time form of
  AllDay ->
    ( ICal.DTStartDate
        { ICal.dtStartOther = def,
          ICal.dtStartDateValue = ICal.Date (date form)
        },
      ICal.DTEndDate
        { ICal.dtEndDateValue = ICal.Date (date form), -- TODO/FIXME: should this be exclusive?
          ICal.dtEndOther = def
        }
    )
  StartEnd {startTime, endTime, timeZone} ->
    let floatingStart =
          Time.LocalTime
            { Time.localDay = date form,
              Time.localTimeOfDay = DP.toStdTimeOfDay startTime
            }
        floatingEnd =
          floatingStart
            { Time.localTimeOfDay = DP.toStdTimeOfDay endTime
            }
        dtStart =
          ICal.ZonedDateTime
            { ICal.dateTimeFloating = floatingStart,
              ICal.dateTimeZone = encodeTZLabel timeZone
            }
        dtEnd =
          ICal.ZonedDateTime
            { ICal.dateTimeFloating = floatingEnd,
              ICal.dateTimeZone = encodeTZLabel timeZone
            }
     in ( ICal.DTStartDateTime
            { ICal.dtStartOther = def,
              ICal.dtStartDateTimeValue = dtStart
            },
          ICal.DTEndDateTime
            { ICal.dtEndDateTimeValue = dtEnd,
              ICal.dtEndOther = def
            }
        )

-- | Patch an existing vevent based on a modification time and event form.
patchVEvent :: Time.UTCTime -> NewEvent -> ICal.VEvent -> ICal.VEvent
patchVEvent utcNow form old =
  let (start, end) = getStartEnd form
   in old
        { ICal.veSummary = patchTextField (ICal.veSummary old) (summary form),
          ICal.veDescription = patchTextField (ICal.veDescription old) (description form),
          ICal.veLocation = patchTextField (ICal.veLocation old) (location form),
          ICal.veDTStart = Just start,
          ICal.veDTEndDuration = Just $ Left end,
          ICal.veLastMod =
            Just
              ICal.LastModified
                { lastModifiedValue = utcNow,
                  lastModifiedOther = def
                },
          ICal.veRRule = S.fromList $ map makeRRule (repeats form)
        }

patchTextField :: FromTextField a => Maybe a -> LT.Text -> Maybe a
patchTextField old newText =
  case old of
    Nothing -> fromTextField newText
    Just old' -> Just $ setTextField newText old'

class FromTextField a where
  fromTextField' :: LT.Text -> a
  setTextField :: LT.Text -> a -> a
  getTextField' :: a -> LT.Text

getTextField :: FromTextField a => Maybe a -> LT.Text
getTextField Nothing = ""
getTextField (Just v) = getTextField' v

fromTextField :: FromTextField a => LT.Text -> Maybe a
fromTextField txt
  | LT.null txt = Nothing
  | otherwise = Just (fromTextField' txt)

instance FromTextField ICal.Summary where
  fromTextField' txt =
    ICal.Summary
      { summaryValue = txt,
        summaryAltRep = def,
        summaryLanguage = def,
        summaryOther = def
      }
  setTextField txt old =
    old {ICal.summaryValue = txt}
  getTextField' = ICal.summaryValue

instance FromTextField ICal.Location where
  fromTextField' txt =
    ICal.Location
      { locationValue = txt,
        locationAltRep = def,
        locationLanguage = def,
        locationOther = def
      }
  setTextField txt old =
    old {ICal.locationValue = txt}
  getTextField' = ICal.locationValue

instance FromTextField ICal.Description where
  fromTextField' txt =
    ICal.Description
      { descriptionValue = txt,
        descriptionAltRep = def,
        descriptionLanguage = def,
        descriptionOther = def
      }
  setTextField txt old =
    old {ICal.descriptionValue = txt}
  getTextField' = ICal.descriptionValue

fromVEvent :: TZ.TZLabel -> ICal.VEvent -> NewEvent
fromVEvent defaultTz ev =
  let dateTimeTz = \case
        ICal.FloatingDateTime _ -> defaultTz
        ICal.UTCDateTime _ -> TZ.Etc__UTC
        ICal.ZonedDateTime {dateTimeZone} ->
          case decodeTZLabel (LT.encodeUtf8 dateTimeZone) of
            Nothing -> defaultTz
            Just tz -> tz
      dateTimeTimeOfDay tz = \case
        ICal.FloatingDateTime dt -> Time.localTimeOfDay dt
        ICal.UTCDateTime utc ->
          Time.localTimeOfDay (TZ.utcToLocalTimeTZ (TZ.tzByLabel tz) utc)
        ICal.ZonedDateTime {dateTimeFloating} ->
          Time.localTimeOfDay dateTimeFloating
   in NewEvent
        { summary = getTextField $ ICal.veSummary ev,
          location = getTextField $ ICal.veLocation ev,
          description = getTextField $ ICal.veDescription ev,
          repeats =
            S.toList (ICal.veRRule ev)
              & map
                ( \ICal.RRule {rRuleValue = ICal.Recur {recurFreq, recurInterval}} ->
                    RepeatRule
                      { frequency = recurFreq,
                        interval = recurInterval
                      }
                ),
          date = case ICal.veDTStart ev of
            Nothing ->
              error "TODO"
            Just (ICal.DTStartDate (ICal.Date day) _) -> day
            Just (ICal.DTStartDateTime val _) -> case val of
              ICal.FloatingDateTime Time.LocalTime {localDay} ->
                localDay
              ICal.UTCDateTime utc ->
                Time.localDay (TZ.utcToLocalTimeTZ TZ.utcTZ utc)
              ICal.ZonedDateTime {dateTimeFloating} ->
                Time.localDay dateTimeFloating,
          time = case ICal.veDTStart ev of
            Nothing ->
              error "TODO"
            Just (ICal.DTStartDate _ _) ->
              AllDay
            Just (ICal.DTStartDateTime val _) ->
              let tz = dateTimeTz val
                  startTime = dateTimeTimeOfDay tz val
               in StartEnd
                    { timeZone = tz,
                      startTime = DP.fromStdTimeOfDay startTime,
                      endTime = case ICal.veDTEndDuration ev of
                        Nothing -> error "TODO"
                        Just (Left end) ->
                          case end of
                            ICal.DTEndDateTime dt _ -> DP.fromStdTimeOfDay $ dateTimeTimeOfDay tz dt
                            ICal.DTEndDate {} -> error "TODO"
                        Just (Right (ICal.DurationProp duration _)) ->
                          -- TODO(cleanup): this is heavily duplicated with Util.Time.addICalDuration;
                          -- just use that. Note that doing that is not 100% mechanical, as that
                          -- gives us a LocalTime, not a TimeOfDay -- we need to decide what to do
                          -- if the day isn't the same. But we should do that anyway, as right now
                          -- we just crash in those cases.
                          let applySign ICal.Positive = id
                              applySign ICal.Negative = negate
                              fixTimeOfDay tod
                                -- FIXME: technically because of leap seconds 60 is legal here,
                                -- but it could also happen on overflow when it's not. We should
                                -- really just be converting to some unit, doing arithmetic and
                                -- converting back, rather than doing all of this ourselves.
                                | Time.todSec tod >= 60 =
                                    fixTimeOfDay
                                      tod
                                        { Time.todSec = Time.todSec tod - 60,
                                          Time.todMin = Time.todMin tod + 1
                                        }
                                | Time.todSec tod < 0 =
                                    fixTimeOfDay
                                      tod
                                        { Time.todSec = Time.todSec tod + 60,
                                          Time.todMin = Time.todMin tod - 1
                                        }
                                | Time.todMin tod >= 60 =
                                    fixTimeOfDay
                                      tod
                                        { Time.todMin = Time.todMin tod - 60,
                                          Time.todHour = Time.todHour tod + 1
                                        }
                                | Time.todMin tod < 0 =
                                    fixTimeOfDay
                                      tod
                                        { Time.todMin = Time.todMin tod + 60,
                                          Time.todHour = Time.todHour tod - 1
                                        }
                                | Time.todHour tod < 0 || Time.todHour tod > 23 =
                                    error "TODO"
                                | otherwise =
                                    tod
                              applyHMS sign h m s =
                                fixTimeOfDay $
                                  Time.TimeOfDay
                                    { todHour = Time.todHour startTime + applySign sign h,
                                      todMin = Time.todMin startTime + applySign sign m,
                                      todSec = Time.todSec startTime + applySign sign (fromIntegral s)
                                    }
                           in DP.fromStdTimeOfDay $ case duration of
                                ICal.DurationDate sign 0 h m s -> applyHMS sign h m s
                                ICal.DurationDate _ _ _ _ _ -> error "TODO"
                                ICal.DurationTime sign h m s -> applyHMS sign h m s
                                ICal.DurationWeek _ 0 -> startTime
                                ICal.DurationWeek _ _ -> error "TODO"
                    }
        }

-- | Convert user-supplied event form into an icalendar event.
--
-- Also accepts as arguments the current time, and a uuid to use for the events'
-- unique id.
toVEvent :: Time.UTCTime -> UUID.UUID -> NewEvent -> ICal.VEvent
toVEvent utcNow uuid form =
  let (start, end) = getStartEnd form
   in ICal.VEvent
        { veUID =
            ICal.UID
              { uidValue = LT.fromStrict $ UUID.toText uuid,
                uidOther = def
              },
          veDTStamp =
            ICal.DTStamp
              { dtStampValue = utcNow,
                dtStampOther = def
              },
          veSummary = fromTextField (summary form),
          veDTStart = Just start,
          veDTEndDuration = Just $ Left end,
          veCreated =
            Just
              ICal.Created
                { createdValue = utcNow,
                  createdOther = def
                },
          veLastMod =
            Just
              ICal.LastModified
                { lastModifiedValue = utcNow,
                  lastModifiedOther = def
                },
          veDescription = fromTextField (description form),
          veRRule = S.fromList $ map makeRRule (repeats form),
          veLocation = fromTextField (location form),
          -- Not used for now:
          veClass = def,
          veGeo = def,
          veOrganizer = def,
          vePriority = def,
          veSeq = def,
          veStatus = def,
          veTransp = def,
          veUrl = def,
          veRecurId = def,
          veAttach = def,
          veAttendee = def,
          veCategories = def,
          veComment = def,
          veContact = def,
          veExDate = def,
          veRStatus = def,
          veRelated = def,
          veResources = def,
          veRDate = def,
          veAlarms = def,
          veOther = def
        }

makeRRule :: RepeatRule -> ICal.RRule
makeRRule RepeatRule {frequency, interval} =
  ICal.RRule
    { rRuleOther = def,
      rRuleValue =
        ICal.Recur
          { recurFreq = frequency,
            recurUntilCount = def,
            recurInterval = interval,
            recurBySecond = def,
            recurByMinute = def,
            recurByHour = def,
            recurByDay = def,
            recurByMonthDay = def,
            recurByYearDay = def,
            recurByWeekNo = def,
            recurByMonth = def,
            recurBySetPos = def,
            recurWkSt = ICal.Monday
            -- Does this matter? Not for our current usage,
            -- but we should research what it means.
          }
    }

encodeTZLabel :: TZ.TZLabel -> LT.Text
encodeTZLabel =
  TZ.toTZName
    >>> LBS.fromStrict
    >>> LT.decodeUtf8With lenientDecode
