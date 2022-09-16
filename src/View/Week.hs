{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module View.Week where

import qualified DB
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import qualified Occurrences as Oc
import qualified Route
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.TZ (TZ)
import qualified Util.TZ as TZ
import View.Common
import Zhp

minutesPerCell :: Int
minutesPerCell = 30

-- | A location on our weekly grid.
data GridLoc = GridLoc
  { -- | The day of the week this goes under.
    dayOfWeek :: Time.DayOfWeek,
    -- | Zero-indexed row of the start of the item, with each row representing
    -- @minutesPerCell@ minutes.
    rowStart :: !Int,
    -- | Number of rows the item takes up.
    rowCount :: !Int
  }
  deriving (Show, Read, Eq)

data MaybeItem
  = Item Item
  | NoItem GridLoc

data Item
  = DayStart Time.DayOfWeek
  | Event EventItem

data EventItem = EventItem
  { eventLoc :: GridLoc,
    eventOccur :: Oc.Occurrence DB.EventEntry
  }

occurRow' :: TZ -> Oc.Occurrence DB.EventEntry -> Int -> (Time.TimeOfDay -> Int) -> Int
occurRow' tz occur def f =
  case Oc.ocTimeInZoneFudge tz (Oc.ocTimeStamp occur) of
    Oc.LocalOCAllDay _ -> def
    Oc.LocalOCAtTime Time.LocalTime {localTimeOfDay} ->
      f localTimeOfDay

occurRowStart :: TZ -> Oc.Occurrence DB.EventEntry -> Int
occurRowStart tz occur = occurRow' tz occur 2 todCells

todCells :: Time.TimeOfDay -> Int
todCells Time.TimeOfDay {todHour, todMin} =
  (todHour * 60 + todMin) `div` minutesPerCell

occurRowCount :: TZ.TZLabel -> Oc.Occurrence DB.EventEntry -> Int
occurRowCount tzLabel occur =
  let event = DB.eeVEvent $ Oc.ocItem occur
      tz = TZ.tzByLabel tzLabel
      start = Oc.ocTimeInZoneFudge tz $ Oc.zonedStartTime tzLabel event
      end = Oc.ocTimeInZoneFudge tz $ Oc.zonedEndTime tzLabel event
      -- XXX: this is a bit messy because this result is actually different than occurRowStart,
      -- which includes an offset into the table. We should clean up this logic at some point.
      startCell = maybe 0 todCells (getTod start)
      endCell = maybe (24 * 60 `div` minutesPerCell) todCells (getTod end)
      getTod (Oc.LocalOCAllDay _) = Nothing
      getTod (Oc.LocalOCAtTime Time.LocalTime {localTimeOfDay = tod}) = Just tod
   in endCell - startCell

dayStyleValue :: Time.DayOfWeek -> Time.DayOfWeek -> String
dayStyleValue startOfWeek day =
  let col = (fromEnum startOfWeek + fromEnum day) `mod` 7
   in "grid-column: " <> show (col + 1) <> ";"

dayStyle :: Time.DayOfWeek -> Time.DayOfWeek -> H.Attribute
dayStyle startOfWeek day =
  A.style $ H.toValue $ dayStyleValue startOfWeek day

locStyle :: Time.DayOfWeek -> GridLoc -> H.Attribute
locStyle startOfWeek GridLoc {dayOfWeek, rowStart, rowCount} =
  -- Add 1 for the heading, and 1 for the fact that css grids start at 1, not 0.
  let start = rowStart + 2
      end = start + rowCount
   in mconcat
        [ A.class_ "week-item",
          A.style $
            H.toValue $
              dayStyleValue startOfWeek dayOfWeek
                <> "grid-row: "
                <> ( if start == end
                       then show start
                       else show start <> " / " <> show end
                   )
        ]

viewItem :: Time.DayOfWeek -> MaybeItem -> H.Html
viewItem startOfWeek (Item (DayStart day)) =
  H.h2
    ! dayStyle startOfWeek day
    ! A.class_ "week-day-heading"
    $ viewDayName day
viewItem startOfWeek (Item (Event EventItem {eventLoc, eventOccur})) =
  H.div ! locStyle startOfWeek eventLoc $ do
    viewOccur eventOccur
viewItem startOfWeek (NoItem loc) =
  H.div ! locStyle startOfWeek loc $
    let minute = rowStart loc * minutesPerCell
     in if minute `mod` 60 == 0
          then viewTimeOfDay (Time.TimeOfDay (minute `div` 60) 0 0)
          else ""

viewDayName :: Time.DayOfWeek -> H.Html
viewDayName day = do
  a
  H.span ! A.class_ "dayname-mid" $ b
  H.span ! A.class_ "dayname-tail" $ c
  where
    (a, b, c) = case day of
      Time.Monday -> ("M", "on", "day")
      Time.Tuesday -> ("T", "ues", "day")
      Time.Wednesday -> ("W", "ed", "nesday")
      Time.Thursday -> ("T", "hu", "rsday")
      Time.Friday -> ("F", "ri", "day")
      Time.Saturday -> ("S", "at", "urday")
      Time.Sunday -> ("S", "un", "day")

viewOccur :: Oc.Occurrence DB.EventEntry -> H.Html
viewOccur Oc.Occurrence {ocItem = DB.EventEntry {eeVEvent, eeId}, ocTimeStamp} =
  H.h3
    $ H.a
      ! A.href
        (H.toValue $ Route.Event eeId (Just ocTimeStamp))
    $ eventSummary eeVEvent

ocDay :: TZ -> Oc.ZonedOCTime -> Time.Day
ocDay tz zot = case Oc.ocTimeInZoneFudge tz zot of
  Oc.LocalOCAllDay day -> day
  Oc.LocalOCAtTime localTime -> Time.localDay localTime

week :: Time.Day -> [LT.Text] -> Time.DayOfWeek -> Oc.ZonedOCTime -> [Oc.Occurrence DB.EventEntry] -> H.Html
week refDay permissions startOfWeek now occurs =
  let tzLabel = Oc.octZone now
      tz = TZ.tzByLabel tzLabel
      title = fromString $ "Week of " <> show (Oc.zonedOCTimeDay now)
      items =
        occurs
          & map (\o -> (ocDay (TZ.tzByLabel $ Oc.octZone now) (Oc.ocTimeStamp o), o))
          & foldl'
            ( \m (d, o) ->
                M.alter
                  ( \case
                      Nothing -> Just [o]
                      Just os -> Just (o : os)
                  )
                  d
                  m
            )
            M.empty
          & M.toList
          & map (\(day, os) -> (fromEnum $ Time.dayOfWeek day, reverse os))
          & M.fromList
          & M.mapWithKey
            ( \dow os ->
                os
                  & map
                    ( \o ->
                        Event
                          EventItem
                            { eventLoc =
                                GridLoc
                                  { dayOfWeek = toEnum dow,
                                    rowStart = occurRowStart tz o,
                                    rowCount = occurRowCount tzLabel o
                                  },
                              eventOccur = o
                            }
                    )
            )
          & ( \m ->
                map
                  ( \d -> case M.lookup (fromEnum d) m of
                      Nothing -> (d, [])
                      Just os -> (d, os)
                  )
                  (take 7 [startOfWeek ..])
            )
          & map (\(d, os) -> DayStart d : os)
          & mconcat
          & fillMissing
   in docToHtml $
        Document
          { permissions,
            title = title,
            body = do
              H.h1 $ H.toHtml title
              weekNav refDay
              H.div
                ! A.class_ "week-grid"
                $ for_ items (viewItem startOfWeek)
              weekNav refDay
          }

weekNav :: Time.Day -> H.Html
weekNav refDay =
  H.nav $ H.ul $ do
    let item diff label =
          H.li $ H.a ! A.href (H.toValue (Route.Week (Time.addDays diff refDay))) $ label
    item (-7) "Previous Week"
    item 7 "Next Week"

-- | Fill in any time slots that don't have events in them with NoItem entries..
fillMissing :: [Item] -> [MaybeItem]
fillMissing = go (toEnum 0) 0
  where
    go dow row (item@(Event ei) : items) =
      let start = rowStart (eventLoc ei)
          count = rowCount (eventLoc ei)
       in if row < start
            then noItem dow row : go dow (row + 1) (item : items)
            else Item item : go dow (start + count) items
    go dow row (item@(DayStart dow') : items) =
      padDay dow row <> (Item item : go dow' 0 items)
    go dow row [] =
      padDay dow row

    padDay dow row =
      if row * minutesPerCell < 24 * 60
        then noItem dow row : padDay dow (row + 1)
        else []

    noItem dow row =
      NoItem
        GridLoc
          { dayOfWeek = dow,
            rowStart = row,
            rowCount = 1
          }
