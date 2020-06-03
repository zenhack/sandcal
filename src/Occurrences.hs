{-# LANGUAGE BangPatterns   #-}
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

eventOccurrences :: Time.UTCTime -> VEvent -> [Occurrence VEvent]
eventOccurrences start ev =
    let rules = map rRuleValue $ Set.toList (veRRule ev)
        streams = map (ruleOccurrences start ev) rules
    in
    Occurrence
        { ocItem = ev
        , ocTimeStamp = getEventStartTime ev
        }
    : merge streams

unboundedOccurrences :: Time.UTCTime -> VEvent -> Recur -> [Occurrence VEvent]
unboundedOccurrences start ev recur =
    expandFreq start ev (recurFreq recur)


expandFreq :: Time.UTCTime -> VEvent -> Frequency -> [Occurrence VEvent]
expandFreq viewStart ev freq =
    case freq of
        Secondly -> iterateNominalDiffTime id
        Minutely -> iterateNominalDiffTime (* 60)
        Hourly -> iterateNominalDiffTime (* (60 * 60))
        Weekly ->
            let octTime' n = case octTime start of
                    LocalOCAllDay day ->
                        LocalOCAllDay $ Time.addDays n day
                    LocalOCAtTime localTime ->
                        LocalOCAtTime localTime
                            { Time.localDay = Time.addDays n (Time.localDay localTime)
                            }
                startIdx = findStartPoint $ \i ->
                    viewStart < zonedOCTimeToUTCFudge (start { octTime = octTime' (fromIntegral i * 7) })
            in
            [ Occurrence
                { ocItem = ev
                , ocTimeStamp = start { octTime = octTime' (fromIntegral (startIdx + n) * 7) }
                }
            | n <- [0..]
            ]
        _ -> [] -- TODO
  where
    start = getEventStartTime ev
    iterateNominalDiffTime toSeconds =
        let toNDF = toSeconds >>> fromIntegral >>> Time.secondsToNominalDiffTime in
        case octTime start of
            LocalOCAllDay _ ->
                -- Shouldn't see this on fractional parts of a day.
                []
            LocalOCAtTime localTime ->
                let atIdx i = start
                        { octTime = LocalOCAtTime $ Time.addLocalTime (toNDF i) localTime
                        }
                    startIdx = findStartPoint $ \i ->
                        viewStart < zonedOCTimeToUTCFudge (atIdx i)
                in
                [0..]
                & map (\i -> Occurrence
                    { ocItem = ev
                    , ocTimeStamp = atIdx (startIdx + i)
                    }
                )

-- | @findStartPoint p@ efficiently finds the first non-negative
-- integer @i@ for which @p i == True@, where @p@ must be monotonically
-- increasing, i.e. if @i > j@ and @p i@, then @p j@.
--
-- Running time is @O(log(firstStartPoint p))@.
findStartPoint :: (Int -> Bool) -> Int
findStartPoint p = go 0 0 1 where
    -- Basic idea of the algorithm is as follows:
    --
    -- * check integers at exponentially increasing intervals until we
    --   find one that satisfies p.
    -- * Go back to the last number we inspected where p returned false.
    -- * Setting the stride back to 1, start counting up again from there (again
    --   exponentially increasing the stride).
    -- * Keep doing this until you see @p i == False && p (i+1) == True@.
    go !last !i !stride
        | p i && i > (last+1) = go last (last+1) 1
        | p i = i
        | otherwise = go i (i+stride) (stride*2)

ruleOccurrences :: Time.UTCTime -> VEvent -> Recur -> [Occurrence VEvent]
ruleOccurrences start vevent recur =
    let unbounded = unboundedOccurrences start vevent recur in
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
