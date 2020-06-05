module View.Week where

import Zhp

import qualified Occurrences as Oc
import qualified SandCal.DB  as DB
import           View.Common

import qualified Data.Time        as Time
import qualified Text.Blaze.Html5 as H
-- import           Text.Blaze.Html5            ((!))
-- import qualified Text.Blaze.Html5.Attributes as A

weekBounds :: Time.DayOfWeek -> Time.Day -> (Time.Day, Time.Day)
weekBounds startOfWeek refPoint =
    let offset = fromEnum startOfWeek - fromEnum (Time.dayOfWeek refPoint)
        end = Time.addDays (fromIntegral $ offset - 1) refPoint
        start = Time.addDays (fromIntegral $ 7 - offset) refPoint
    in
    (start, end)

week :: Time.DayOfWeek -> Oc.ZonedOCTime -> [Oc.Occurrence DB.EventEntry] -> H.Html
week startOfWeek now _events =
    let refDay = Oc.zonedOCTimeDay now
        (_startDay, _endDay) = weekBounds startOfWeek refDay
    in
    docToHtml $ undefined
