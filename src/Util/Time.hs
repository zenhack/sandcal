-- | Misc. helpers related to dates & times.
--
-- Things in this module might conceptually fit in a general purpose time
-- library, but do not appear to be covered by the @time@ package.
{-# LANGUAGE NamedFieldPuns #-}
module Util.Time
    ( weekBounds
    , startOfDay
    , endOfDay
    , addICalDuration
    ) where

import Zhp

import qualified Data.Time as Time
import qualified Util.ICal as ICal

-- | @'weekBounds' startOfWeek refPoint@ finds the boundaries of the week
-- containing @refPoint@, assuming the week starts on @startOfWeek@. Returns
-- @(start, end)@, where @start@ is the first day of the week containing
-- @refPoint@ and @end@ is the last.
weekBounds :: Time.DayOfWeek -> Time.Day -> (Time.Day, Time.Day)
weekBounds startOfWeek refPoint =
    let rawOffset = fromEnum (Time.dayOfWeek refPoint) - fromEnum startOfWeek
        offset = if rawOffset < 0
            then rawOffset + 7
            else rawOffset `mod` 7
        start = Time.addDays (fromIntegral $ -offset) refPoint
        end = Time.addDays 6 start
    in
    (start, end)

-- | Get the start time of a day.
startOfDay :: Time.Day -> Time.LocalTime
startOfDay day = Time.LocalTime
    { Time.localDay = day
    , Time.localTimeOfDay = Time.TimeOfDay 0 0 0
    }

-- | Get end time of a day, ignoring leap seconds (i.e. assume there is no
-- leap second).
endOfDay :: Time.Day -> Time.LocalTime
endOfDay day = Time.LocalTime
    { Time.localDay = day
    , Time.localTimeOfDay = Time.TimeOfDay 23 59 59
    }


addICalDuration :: ICal.Duration -> Time.LocalTime -> Time.LocalTime
addICalDuration duration start =
    let applySign ICal.Positive = id
        applySign ICal.Negative = negate
        fixLocalTime lt@Time.LocalTime{Time.localDay = day, Time.localTimeOfDay = tod}
            -- FIXME: technically because of leap seconds 60 is legal here,
            -- but it could also happen on overflow when it's not. We should
            -- really just be converting to some unit, doing arithmetic and
            -- converting back, rather than doing all of this ourselves.
            | Time.todSec tod >= 60 = fixLocalTime lt
                { Time.localTimeOfDay = tod
                    { Time.todSec = Time.todSec tod - 60
                    , Time.todMin = Time.todMin tod + 1
                    }
                }
            | Time.todSec tod < 0 = fixLocalTime lt
                { Time.localTimeOfDay = tod
                    { Time.todSec = Time.todSec tod + 60
                    , Time.todMin = Time.todMin tod - 1
                    }
                }
            | Time.todMin tod >= 60 = fixLocalTime lt
                { Time.localTimeOfDay = tod
                    { Time.todMin = Time.todMin tod - 60
                    , Time.todHour = Time.todHour tod + 1
                    }
                }
            | Time.todMin tod < 0 = fixLocalTime lt
                { Time.localTimeOfDay = tod
                    { Time.todMin = Time.todMin tod + 60
                    , Time.todHour = Time.todHour tod - 1
                    }
                }
            | Time.todHour tod < 0 = fixLocalTime lt
                { Time.localDay = Time.addDays (-1) day
                , Time.localTimeOfDay = tod { Time.todHour = Time.todHour tod + 24 }
                }
            | Time.todHour tod > 23 = fixLocalTime lt
                { Time.localDay = Time.addDays 1 day
                , Time.localTimeOfDay = tod { Time.todHour = Time.todHour tod - 24 }
                }
            | otherwise =
                lt
        applyDHMS :: ICal.Sign -> Integer -> Int -> Int -> Int -> Time.LocalTime
        applyDHMS sign d h m s =
            let Time.LocalTime{Time.localDay = day, Time.localTimeOfDay = tod} = start in
            fixLocalTime $ Time.LocalTime
                { Time.localTimeOfDay = Time.TimeOfDay
                    { Time.todHour = Time.todHour tod + applySign sign h
                    , Time.todMin = Time.todMin tod + applySign sign m
                    , Time.todSec = Time.todSec tod + applySign sign (fromIntegral s)
                    }
                , Time.localDay = Time.addDays (applySign sign d) day
                }
    in
    case duration of
        ICal.DurationDate sign d h m s -> applyDHMS sign (fromIntegral d) h m s
        ICal.DurationTime sign h m s   -> applyDHMS sign 0 h m s
        ICal.DurationWeek sign w       -> applyDHMS sign (fromIntegral $ 7 * w) 0 0 0
