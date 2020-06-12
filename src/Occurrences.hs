{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Occurrences
    ( Occurrence(..)
    , LocalOCTime(..)
    , ZonedOCTime(..)
    , eventOccurrences
    , zonedOCTimeDay
    , zonedOCTimeToUTCFudge
    , ocTimeInZoneFudge
    , merge
    ) where

import Zhp

import ICal

import           Data.Time.Zones     (TZ)
import qualified Data.Time.Zones     as TZ
import qualified Data.Time.Zones.All as TZ

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Set                as Set
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Time               as Time

data LocalOCTime
    = LocalOCAllDay Time.Day
    | LocalOCAtTime Time.LocalTime
    deriving(Show, Eq, Ord)

modifyLocalOCDay :: (Time.Day -> Time.Day) -> LocalOCTime ->  LocalOCTime
modifyLocalOCDay f = \case
    LocalOCAllDay day ->
        LocalOCAllDay (f day)
    LocalOCAtTime localTime ->
        LocalOCAtTime localTime
            { Time.localDay = f (Time.localDay localTime)
            }

data ZonedOCTime = ZonedOCTime
    { octZone :: TZ
    , octTime :: LocalOCTime
    }
    deriving(Show)

-- | Convert a ZonedOCTime to a LocalOCTime in the specified timezone.
--
-- Leaves LocalOCAllDay unchanged.
--
-- See also the notes about invalid/ambiguous times re: 'zonedOCTimeTOUTCFudge'.
ocTimeInZoneFudge :: TZ -> ZonedOCTime -> LocalOCTime
ocTimeInZoneFudge tz zot = case octTime zot of
    LocalOCAllDay day -> LocalOCAllDay day
    LocalOCAtTime localTime ->
        let utc = TZ.localTimeToUTCTZ (octZone zot) localTime
            local' = TZ.utcToLocalTimeTZ tz utc
        in
        LocalOCAtTime local'


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

-- | @'zonedStartTime' defaultTz event@ is the start time of an event
-- with a time zone.
--
-- It tries to extract a sensible time zone from the event itself,
-- but if it is unable to do so, it assumes @defaultTz@.
zonedStartTime :: TZ -> VEvent -> ZonedOCTime
zonedStartTime defaultTz ev =
    case veDTStart ev of
        Nothing ->
            zonedOCTimeFromUTC $ dtStampValue (veDTStamp ev)
        Just (DTStartDate (Date day) _) ->
            ZonedOCTime
                { octZone = defaultTz
                , octTime = LocalOCAllDay day
                }
        Just DTStartDateTime{dtStartDateTimeValue = FloatingDateTime localTime} ->
            ZonedOCTime
                { octZone = defaultTz
                , octTime = LocalOCAtTime localTime
                }
        Just DTStartDateTime{dtStartDateTimeValue = UTCDateTime utcTime} ->
            zonedOCTimeFromUTC utcTime
        Just DTStartDateTime{dtStartDateTimeValue = ZonedDateTime{dateTimeFloating, dateTimeZone}} ->
            let tz = case TZ.tzByName $ LBS.toStrict $ LTE.encodeUtf8 dateTimeZone of
                    Nothing   -> defaultTz
                    Just zone -> zone
            in
            ZonedOCTime
                { octZone = tz
                , octTime = LocalOCAtTime dateTimeFloating
                }

merge :: [[Occurrence a]] -> [Occurrence a]
merge = mergeManyOn (ocTimeStamp >>> zonedOCTimeToUTCFudge)

mergeManyOn :: Ord b => (a -> b) -> [[a]] -> [a]
mergeManyOn f lists =
    case lists of
        [] -> []
        [x] -> x
        _ ->
            let (xs, ys) = split lists in
            mergeOn f (mergeManyOn f xs) (mergeManyOn f ys)
  where
    split [] = ([], [])
    split [x] = ([x], [])
    split (x:y:zs) =
        let (xs, ys) = split zs in
        (x:xs, y:ys)

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ xs [] = xs
mergeOn _ [] ys = ys
mergeOn f (x:xs) (y:ys)
    | f x < f y = x : mergeOn f xs (y:ys)
    | otherwise = y : mergeOn f (x:xs) ys

eventOccurrences :: TZ -> Time.UTCTime -> VEvent -> [Occurrence VEvent]
eventOccurrences defaultTz start ev =
    let rules = map rRuleValue $ Set.toList (veRRule ev)
        -- TODO(cleanup): refactor to just pass eventStartTime into
        -- ruleOccurrences, rather than re-computing it.
        streams = map (ruleOccurrences defaultTz start ev) rules
        eventStartTime = zonedStartTime defaultTz ev
        hd =
            [ Occurrence
                { ocItem = ev
                , ocTimeStamp = eventStartTime
                }
            | zonedOCTimeToUTCFudge eventStartTime >= start
            ]
    in
    hd <> merge streams

unboundedOccurrences :: TZ -> Time.UTCTime -> VEvent -> Recur -> [Occurrence VEvent]
unboundedOccurrences defaultTz start ev recur =
    expandFreq defaultTz start ev (recurFreq recur) (recurInterval recur)


expandFreq :: TZ -> Time.UTCTime -> VEvent -> Frequency -> Int -> [Occurrence VEvent]
expandFreq defaultTz viewStart ev freq interval =
    case freq of
        Secondly -> expandSeconds id
        Minutely -> expandSeconds (* 60)
        Hourly   -> expandSeconds (* (60 * 60))
        Daily    -> expandDays id
        Weekly   -> expandDays (* 7)
        Monthly  -> expandModifyDay Time.addGregorianMonthsClip
        Yearly   -> expandModifyDay Time.addGregorianYearsClip
  where
    start = zonedStartTime defaultTz ev
    expandIndex atIdx =
        let startIdx =
                findStartPoint $ \i -> viewStart < zonedOCTimeToUTCFudge (atIdx i)
        in
        [ Occurrence
            { ocItem = ev
            , ocTimeStamp = atIdx (startIdx + (n * interval))
            }
        | n <- [0..]
        ]
    expandSeconds toSeconds =
        let toNDF = toSeconds >>> fromIntegral >>> Time.secondsToNominalDiffTime in
        case octTime start of
            LocalOCAllDay _ ->
                -- Shouldn't see this on fractional parts of a day.
                []
            LocalOCAtTime localTime ->
                expandIndex $ \i -> start
                    { octTime = LocalOCAtTime $ Time.addLocalTime (toNDF i) localTime
                    }
    expandModifyDay modify = expandIndex $ \i -> start
        { octTime = modifyLocalOCDay (modify $ fromIntegral i) (octTime start) }
    expandDays toNDays =
        expandModifyDay $ Time.addDays . toNDays

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

ruleOccurrences :: TZ -> Time.UTCTime -> VEvent -> Recur -> [Occurrence VEvent]
ruleOccurrences defaultTz start vevent recur =
    let unbounded = unboundedOccurrences defaultTz start vevent recur in
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
