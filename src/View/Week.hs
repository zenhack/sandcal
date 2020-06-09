{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module View.Week where

import Zhp

import qualified Occurrences as Oc
import qualified SandCal.DB  as DB
import           View.Common

import qualified Data.Map.Strict  as M
import qualified Data.Time        as Time
import           Data.Time.Zones  (TZ)
import qualified Data.Time.Zones  as Tz
import qualified Text.Blaze.Html5 as H
-- import qualified Util.Time           as UT
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as A

minutesPerCell :: Int
minutesPerCell = 30

data GridLoc = GridLoc
    { dayOfWeek :: Time.DayOfWeek
    , rowStart  :: !Int
    , rowEnd    :: !Int
    }

data Item
    = DayStart Time.DayOfWeek
    | Event
        { eventLoc   :: GridLoc
        , eventOccur :: Oc.Occurrence DB.EventEntry
        }

occurRow' :: TZ -> Oc.Occurrence DB.EventEntry -> Int -> (Time.TimeOfDay -> Int) -> Int
occurRow' tz occur def f =
    case Oc.ocTimeInZoneFudge tz (Oc.ocTimeStamp occur) of
        Oc.LocalOCAllDay _ -> def
        Oc.LocalOCAtTime Time.LocalTime{localTimeOfDay} ->
            f localTimeOfDay

occurRowStart :: TZ -> Oc.Occurrence DB.EventEntry -> Int
occurRowStart tz occur =
    occurRow' tz occur 1 $ \Time.TimeOfDay{todHour,todMin} ->
        (todHour * 60 + todMin) `div` minutesPerCell

occurRowEnd :: TZ -> Oc.Occurrence DB.EventEntry -> Int
occurRowEnd tz occur =
    -- TODO: fill this in properly
    occurRowStart tz occur + 1

dayStyleValue :: Time.DayOfWeek -> Time.DayOfWeek -> String
dayStyleValue startOfWeek day =
    let col = (fromEnum startOfWeek + fromEnum day) `mod` 7 in
    "grid-column: "  <> show (col + 1) <> ";"

dayStyle :: Time.DayOfWeek -> Time.DayOfWeek -> H.Attribute
dayStyle startOfWeek day =
    A.style $ H.toValue $ dayStyleValue startOfWeek day

locStyle :: Time.DayOfWeek -> GridLoc -> H.Attribute
locStyle startOfWeek GridLoc{dayOfWeek, rowStart, rowEnd} = mconcat
    [ A.class_ "week-item"
    , A.style $ H.toValue $
        dayStyleValue startOfWeek dayOfWeek <>
        "grid-row: " <>
            (if rowStart == rowEnd
                then show rowStart
                else show rowStart <> " / " <> show rowEnd
            )
    ]

viewItem :: Time.DayOfWeek -> Item -> H.Html
viewItem startOfWeek (DayStart day) =
    H.h2
        ! dayStyle startOfWeek day
        ! A.class_ "week-day-heading"
        $ H.toHtml $ show day
viewItem startOfWeek Event{eventLoc, eventOccur} =
    H.div ! locStyle startOfWeek eventLoc $ do
        viewOccur eventOccur


viewOccur :: Oc.Occurrence DB.EventEntry -> H.Html
viewOccur Oc.Occurrence{ocItem = DB.EventEntry{eeVEvent}} =
    H.h3 $ eventSummary eeVEvent

ocDay :: TZ -> Oc.ZonedOCTime -> Time.Day
ocDay tz zot = case Oc.octTime zot of
    Oc.LocalOCAllDay day -> day
    Oc.LocalOCAtTime localTime ->
        Tz.localTimeToUTCTZ (Oc.octZone zot) localTime
            & Tz.utcToLocalTimeTZ tz
            & Time.localDay


week :: Time.DayOfWeek -> Oc.ZonedOCTime -> [Oc.Occurrence DB.EventEntry] -> H.Html
week startOfWeek now occurs =
    let tz = Oc.octZone now
        items = occurs
            & map (\o -> (ocDay (Oc.octZone now) (Oc.ocTimeStamp o), o))
            & foldl' (\m (d, o) ->
                    M.alter
                        (\case
                            Nothing -> Just [o]
                            Just os -> Just (o:os)
                        )
                        d
                        m
                )
                M.empty
            & M.toList
            & map (\(day, os) -> (fromEnum $ Time.dayOfWeek day, reverse os))
            & M.fromList
            & M.mapWithKey
                (\dow os ->
                    os & map (\o ->
                        Event
                            { eventLoc = GridLoc
                                { dayOfWeek = toEnum dow
                                , rowStart = occurRowStart tz o
                                , rowEnd = occurRowEnd tz o
                                }
                            , eventOccur = o
                            }
                    )
                )
            & M.toList
            & map (\(d, os) -> DayStart (toEnum d) : os)
            & mconcat
    in
    docToHtml $ Document
        { title = fromString $ "Week of " <> show (Oc.zonedOCTimeDay now)
        , body = H.div
            ! A.class_ "week-grid"
            $ for_ items (viewItem startOfWeek)
        }
