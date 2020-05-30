{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Occurrences
    ( Occurrence(..)
    , LocalOCTime(..)
    , ZonedOCTime(..)
    , eventOccurrences
    , zonedOCTimeDay
    , merge
    , dropBeforeUTC
    ) where

import Zhp

import ICal

import           Data.Time.Zones     (TZ)
import qualified Data.Time.Zones     as TZ
import qualified Data.Time.Zones.All as TZ

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Set                as Set
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Time               as Time

data LocalOCTime
    = LocalOCAllDay Time.Day
    | LocalOCAtTime Time.LocalTime
    deriving(Show, Eq, Ord)

data ZonedOCTime = ZonedOCTime
    { octZone :: TZ
    , octTime :: LocalOCTime
    }
    deriving(Show)

-- | Convert a ZonedOCTime to a UTCTime.
--
-- Treats all-day events as starting at 00:00.
--
-- For times that are ambiguous or invalid because of timezone changes
-- (DST being the most obvious), we just fudge it and pick the earliest
-- reasonable interpretation. Accordingly, this should *not* be used
-- anywhere where the distinction is critical.
zonedOCTimeToUTCFudge :: ZonedOCTime -> Time.UTCTime
zonedOCTimeToUTCFudge zot =
    let localTime = case octTime zot of
            LocalOCAtTime time -> time
            LocalOCAllDay day  -> Time.LocalTime day (Time.TimeOfDay 0 0 0)
    in
    TZ.localTimeToUTCTZ (octZone zot) localTime

data Occurrence a = Occurrence
    { ocItem      :: a
    , ocTimeStamp :: ZonedOCTime
    }
    deriving(Functor, Show)

data StartTimeError
    = FloatingStartTime
    | BadTimeZone LT.Text
    deriving(Show)

zonedOCTimeDay :: ZonedOCTime -> Time.Day
zonedOCTimeDay zot = case octTime zot of
    LocalOCAllDay day       -> day
    LocalOCAtTime localTime -> Time.localDay localTime

zonedOCTimeFromUTC :: Time.UTCTime -> ZonedOCTime
zonedOCTimeFromUTC utcTime = ZonedOCTime
    { octZone = TZ.utcTZ
    , octTime = LocalOCAtTime $
        Time.utcToLocalTime Time.utc utcTime
    }

getEventStartTime :: VEvent -> ZonedOCTime
getEventStartTime ev = case eventDTStart ev of
    Right t -> t
    Left _  -> zonedOCTimeFromUTC $ dtStampValue (veDTStamp ev)

eventDTStart :: VEvent -> Either StartTimeError ZonedOCTime
eventDTStart ev =
    case veDTStart ev of
        Nothing ->
            Right $ zonedOCTimeFromUTC $ dtStampValue (veDTStamp ev)
        Just (DTStartDate _ _) ->
            Left FloatingStartTime
        Just DTStartDateTime{dtStartDateTimeValue = FloatingDateTime _localTime} ->
            Left FloatingStartTime
        Just DTStartDateTime{dtStartDateTimeValue = UTCDateTime utcTime} ->
            Right $ zonedOCTimeFromUTC utcTime
        Just DTStartDateTime{dtStartDateTimeValue = ZonedDateTime{dateTimeFloating, dateTimeZone}} ->
            case TZ.tzByName $ LBS.toStrict $ LTE.encodeUtf8 dateTimeZone of
                Nothing -> Left $ BadTimeZone dateTimeZone
                Just tz -> Right $ ZonedOCTime
                    { octZone = tz
                    , octTime = LocalOCAtTime dateTimeFloating
                    }

merge :: [[Occurrence a]] -> [Occurrence a]
merge = foldl' (mergeOn (ocTimeStamp >>> zonedOCTimeToUTCFudge)) []

dropBeforeUTC :: Time.UTCTime -> [Occurrence a] -> [Occurrence a]
dropBeforeUTC utc = dropWhile $ \Occurrence{ocTimeStamp} ->
    zonedOCTimeToUTCFudge ocTimeStamp < utc

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ xs [] = xs
mergeOn _ [] ys = ys
mergeOn f (x:xs) (y:ys)
    | f x < f y = x : mergeOn f xs (y:ys)
    | otherwise = y : mergeOn f (x:xs) ys

eventOccurrences :: VEvent -> [Occurrence VEvent]
eventOccurrences ev =
    let rules = map rRuleValue $ Set.toList (veRRule ev)
        streams = map (ruleOccurrences ev) rules
    in
    Occurrence
        { ocItem = ev
        , ocTimeStamp = getEventStartTime ev
        }
    : merge streams

unboundedOccurrences :: VEvent -> Recur -> [Occurrence VEvent]
unboundedOccurrences ev recur =
    expandFreq ev (recurFreq recur)

expandFreq :: VEvent -> Frequency -> [Occurrence VEvent]
expandFreq ev freq =
    case freq of
        Secondly -> iterateNominalDiffTime Time.secondsToNominalDiffTime
        Minutely -> iterateNominalDiffTime ((* 60) >>> Time.secondsToNominalDiffTime)
        Hourly -> iterateNominalDiffTime ((* (60 * 60)) >>> Time.secondsToNominalDiffTime)
        Weekly ->
            let octTime' n = case octTime start of
                    LocalOCAllDay day ->
                        LocalOCAllDay $ Time.addDays n day
                    LocalOCAtTime localTime ->
                        LocalOCAtTime localTime
                            { Time.localDay = Time.addDays n (Time.localDay localTime)
                            }
            in
            [ Occurrence
                { ocItem = ev
                , ocTimeStamp = start { octTime = octTime' n }
                }
            | n <- [7,14..]
            ]
        _ -> [] -- TODO
  where
    start = getEventStartTime ev
    iterateNominalDiffTime toNDF =
        case octTime start of
            LocalOCAllDay _ ->
                -- Shouldn't see this on fractional parts of a day.
                []
            LocalOCAtTime localTime ->
                [1..]
                & map (\n -> Occurrence
                    { ocItem = ev
                    , ocTimeStamp = start
                        { octTime = LocalOCAtTime $ Time.addLocalTime (toNDF n) localTime
                        }
                    }
                )

ruleOccurrences :: VEvent -> Recur -> [Occurrence VEvent]
ruleOccurrences vevent recur =
    let unbounded = unboundedOccurrences vevent recur in
    case recurUntilCount recur of
        Nothing                      -> unbounded
        Just (Right count)           -> take count unbounded
        Just (Left dateAndMaybeTime) -> takeWhile (matchesUntil dateAndMaybeTime) unbounded


matchesUntil :: Either Date DateTime -> Occurrence a -> Bool
matchesUntil limit Occurrence{ocTimeStamp = zot} = case limit of
    Left Date{dateValue} -> zonedOCTimeDay zot < dateValue
    Right (UTCDateTime utc) -> zonedOCTimeToUTCFudge zot < utc
    Right (FloatingDateTime lt) -> case octTime zot of
        LocalOCAllDay day -> day < Time.localDay lt
        LocalOCAtTime lt' -> lt' < lt
    Right ZonedDateTime{dateTimeFloating = lt} ->
        case octTime zot of
            -- FIXME: take the time zone into account, and get rid of
            -- copypasta from above.
            LocalOCAllDay day -> day < Time.localDay lt
            LocalOCAtTime lt' -> lt' < lt
