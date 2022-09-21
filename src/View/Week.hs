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

data DayData a = DayData
  { day :: Time.DayOfWeek,
    items :: [Item a]
  }

data Item a = Item
  { itemLoc :: GridLoc,
    itemValue :: a
  }

viewDay :: Time.DayOfWeek -> DayData (Maybe (Oc.Occurrence DB.EventEntry)) -> H.Html
viewDay startOfWeek DayData {day, items} = do
  H.h2
    ! dayStyle startOfWeek day
    ! A.class_ "week-day-heading"
    $ viewDayName day
  traverse_ (viewItem day) items

viewItem :: Time.DayOfWeek -> Item (Maybe (Oc.Occurrence DB.EventEntry)) -> H.Html
viewItem startOfWeek (Item loc val) =
  case val of
    Just occur ->
      H.div ! locStyle startOfWeek loc $ viewOccur occur
    Nothing ->
      H.div ! locStyle startOfWeek loc $
        let minute = rowStart loc * minutesPerCell
         in if minute `mod` 60 == 0
              then viewTimeOfDay (Time.TimeOfDay (minute `div` 60) 0 0)
              else ""

occurRow' :: TZ -> Oc.Occurrence DB.EventEntry -> Int -> (Time.TimeOfDay -> Int) -> Int
occurRow' tz occur def f =
  case Oc.ocTimeInZoneFudge tz (Oc.ocTimeStamp occur) of
    Oc.LocalOCAllDay _ -> def
    Oc.LocalOCAtTime Time.LocalTime {localTimeOfDay} ->
      f localTimeOfDay

occurRowStart :: TZ -> Oc.Occurrence DB.EventEntry -> Int
occurRowStart tz occur = occurRow' tz occur 0 todCells

todCells :: Time.TimeOfDay -> Int
todCells Time.TimeOfDay {todHour, todMin} =
  (todHour * 60 + todMin) `divRoundUp` minutesPerCell
  where
    x `divRoundUp` y =
      let ret = x `div` y
       in if x `mod` y == 0
            then ret
            else (ret + 1)

occurRowCount :: TZ.TZLabel -> Oc.Occurrence DB.EventEntry -> Int
occurRowCount tzLabel occur =
  let event = DB.eeVEvent $ Oc.ocItem occur
      tz = TZ.tzByLabel tzLabel
      start = Oc.ocTimeInZoneFudge tz $ Oc.zonedStartTime tzLabel event
      end = Oc.ocTimeInZoneFudge tz $ Oc.zonedEndTime tzLabel event
      startCell = maybe 0 todCells (getTod start)
      endCell = maybe (24 * 60 `div` minutesPerCell) todCells (getTod end)
      getTod (Oc.LocalOCAllDay _) = Nothing
      getTod (Oc.LocalOCAtTime Time.LocalTime {localTimeOfDay = tod}) = Just tod
   in endCell - startCell

dayColumn :: Time.DayOfWeek -> Time.DayOfWeek -> Int
dayColumn startOfWeek day =
  (fromEnum startOfWeek + fromEnum day) `mod` 7

dayStyleValue :: Time.DayOfWeek -> Time.DayOfWeek -> String
dayStyleValue startOfWeek day =
  let col = dayColumn startOfWeek day
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
        [ A.class_ $
            fromString $
              "week-item week-item--col-"
                <> show (dayColumn startOfWeek dayOfWeek),
          A.style $
            H.toValue $
              dayStyleValue startOfWeek dayOfWeek
                <> "grid-row: "
                <> ( if start == end
                       then show start
                       else show start <> " / " <> show end
                   )
        ]

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
viewOccur Oc.Occurrence {ocItem = DB.EventEntry {eeVEvent, eeId}, ocTimeStamp} = do
  H.a
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
      days =
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
          & map (\(day, os) -> (Time.dayOfWeek day, reverse os))
          & M.fromList
          & M.mapWithKey
            ( \dow os ->
                os
                  & map
                    ( \o ->
                        Item
                          { itemLoc =
                              GridLoc
                                { dayOfWeek = dow,
                                  rowStart = occurRowStart tz o,
                                  rowCount = occurRowCount tzLabel o
                                },
                            itemValue = o
                          }
                    )
            )
          & ( \m ->
                map
                  ( \d -> case M.lookup d m of
                      Nothing -> (d, [])
                      Just os -> (d, os)
                  )
                  ( [0 .. 6]
                      & map toEnum
                      & cycle
                      & drop (fromEnum startOfWeek)
                      & take 7
                  )
            )
          & map
            ( \(d, os) ->
                DayData
                  { day = d,
                    items = fillMissing d os
                  }
            )
   in docToHtml $
        Document
          { permissions,
            title = title,
            body = do
              H.h1 $ H.toHtml title
              weekNav refDay
              H.div
                ! A.class_ "week-grid"
                $ for_ days (viewDay startOfWeek)
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
fillMissing :: Time.DayOfWeek -> [Item a] -> [Item (Maybe a)]
fillMissing dow = go 0
  where
    go row (item@Item {itemLoc, itemValue} : items) =
      let start = rowStart itemLoc
          count = rowCount itemLoc
       in if row < start
            then noItem row : go (row + 1) (item : items)
            else
              Item {itemLoc, itemValue = Just itemValue}
                : go (start + count) items
    go row [] =
      padDay row

    padDay row =
      if row * minutesPerCell < 24 * 60
        then noItem row : padDay (row + 1)
        else []

    noItem row =
      Item
        GridLoc
          { dayOfWeek = dow,
            rowStart = row,
            rowCount = 1
          }
        Nothing
