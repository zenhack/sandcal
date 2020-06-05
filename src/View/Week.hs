module View.Week where

import Zhp

import qualified Occurrences as Oc
import qualified SandCal.DB  as DB
import           View.Common

import qualified Data.Time        as Time
import qualified Text.Blaze.Html5 as H
import qualified Util.Time        as UT
-- import           Text.Blaze.Html5            ((!))
-- import qualified Text.Blaze.Html5.Attributes as A

week :: Time.DayOfWeek -> Oc.ZonedOCTime -> [Oc.Occurrence DB.EventEntry] -> H.Html
week startOfWeek now _events =
    let refDay = Oc.zonedOCTimeDay now
        (_startDay, _endDay) = UT.weekBounds startOfWeek refDay
    in
    docToHtml $ undefined
