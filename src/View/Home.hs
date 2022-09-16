{-# LANGUAGE NamedFieldPuns #-}

module View.Home
  ( home,
  )
where

import qualified DB
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import qualified Occurrences as Oc
import qualified Route
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.ICal (veventDuration)
import Util.TZ (TZLabel, tzByLabel)
import Util.Time (addICalDuration)
import View.Common
import Zhp

data EventSet = EventSet
  { esDay :: Time.Day,
    esOccurrences :: [Oc.Occurrence DB.EventEntry]
  }

makeEventSets :: [Oc.Occurrence DB.EventEntry] -> [EventSet]
makeEventSets [] = []
makeEventSets os@(Oc.Occurrence {Oc.ocTimeStamp} : _) =
  let day = Oc.zonedOCTimeDay ocTimeStamp
   in go (EventSet day []) os
  where
    finalizeEs :: EventSet -> EventSet
    finalizeEs es = es {esOccurrences = reverse (esOccurrences es)}
    go e [] = [finalizeEs e]
    go e (o@Oc.Occurrence {Oc.ocTimeStamp} : os) =
      let day' = Oc.zonedOCTimeDay ocTimeStamp
       in if esDay e /= day'
            then finalizeEs e : go (EventSet day' [o]) os
            else go e {esOccurrences = o : esOccurrences e} os

normalizeTime :: TZLabel -> Oc.Occurrence a -> Oc.Occurrence a
normalizeTime targetZone oc@Oc.Occurrence {Oc.ocTimeStamp = zot} =
  let localTime = Oc.ocTimeInZoneFudge (tzByLabel targetZone) zot
   in oc
        { Oc.ocTimeStamp =
            Oc.ZonedOCTime
              { Oc.octTime = localTime,
                Oc.octZone = targetZone
              }
        }

viewEventSet :: Time.Day -> EventSet -> H.Html
viewEventSet today es = do
  let day = esDay es
  H.h2 ! A.class_ "upcomingDayHeading" $ do
    H.toHtml $
      Time.formatTime
        Time.defaultTimeLocale
        "%a %e %b %Y"
        day
    when (day == today) " (Today)"
    when (day == succ today) " (Tomorrow)"
  for_ (esOccurrences es) $ \Oc.Occurrence {Oc.ocItem, Oc.ocTimeStamp = zot} -> do
    let vEvent = DB.eeVEvent ocItem
        title = eventSummary vEvent
        timeStamp = H.span ! A.class_ "eventTime" $
          case Oc.octTime zot of
            Oc.LocalOCAllDay _ -> "All Day"
            Oc.LocalOCAtTime lt@Time.LocalTime {Time.localTimeOfDay} -> do
              viewLocalTimeOfDay localTimeOfDay
              for_ (veventDuration vEvent) $ \dur -> do
                let lt' = addICalDuration dur lt
                " - "
                viewLocalTimeOfDay (Time.localTimeOfDay lt')
    H.div ! A.class_ "upcomingEvent" $ do
      H.p $ timeStamp
      H.p
        $ H.a
          ! A.href (H.toValue $ Route.Event (DB.eeId ocItem) (Just zot))
        $ title

viewLocalTimeOfDay :: Time.TimeOfDay -> H.Html
viewLocalTimeOfDay localTimeOfDay =
  H.time
    ! A.class_ "eventTime"
    ! A.datetime (H.toValue $ show localTimeOfDay)
    $ viewTimeOfDay localTimeOfDay

home :: [LT.Text] -> Time.Day -> TZLabel -> [Oc.Occurrence DB.EventEntry] -> H.Html
home permissions today targetZone entries =
  docToHtml
    Document
      { permissions,
        title = "Upcoming Events",
        body = do
          H.h1 "Upcoming Events"
          traverse_ (viewEventSet today) (makeEventSets $ map (normalizeTime targetZone) entries)
      }
