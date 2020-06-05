-- | Misc. helpers related to dates & times.
--
-- Things in this module might conceptually fit in a general purpose time
-- library, but do not appear to be covered by the @time@ package.
module Util.Time
    ( weekBounds
    ) where

import Zhp

import qualified Data.Time as Time

-- | @'weekBounds' startOfWeek refPoint@ finds the boundaries of the week
-- containing @refPoint@, assuming the week starts on @startOfWeek@. Returns
-- @(start, end)@, where @start@ is the first day of the week containing
-- @refPoint@ and @end@ is the last.
weekBounds :: Time.DayOfWeek -> Time.Day -> (Time.Day, Time.Day)
weekBounds startOfWeek refPoint =
    let offset = fromEnum startOfWeek - fromEnum (Time.dayOfWeek refPoint)
        end = Time.addDays (fromIntegral $ offset - 1) refPoint
        start = Time.addDays (fromIntegral $ 7 - offset) refPoint
    in
    (start, end)
