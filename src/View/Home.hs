{-# LANGUAGE NamedFieldPuns #-}
module View.Home
    ( home
    ) where

import Zhp

import View.Common

import qualified DB
import qualified Occurrences as Oc
import qualified Route

import qualified Data.Text.Lazy              as LT
import qualified Data.Time                   as Time
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Util.TZ                     (TZ)

data Item
    = DayHeading Time.Day
    | Occurrence (Oc.Occurrence DB.EventEntry)

makeItems :: [Oc.Occurrence DB.EventEntry] -> [Item]
makeItems [] = []
makeItems (o@Oc.Occurrence{Oc.ocTimeStamp}:os) =
    let day = Oc.zonedOCTimeDay ocTimeStamp in
    DayHeading day : Occurrence o : makeItems' day os

makeItems' :: Time.Day -> [Oc.Occurrence DB.EventEntry] -> [Item]
makeItems' _ [] = []
makeItems' day (o@Oc.Occurrence{Oc.ocTimeStamp}:os) =
    let day' = Oc.zonedOCTimeDay ocTimeStamp in
    if day /= day' then
        DayHeading day' : Occurrence o : makeItems' day' os
    else
        Occurrence o : makeItems' day os

viewItem :: Time.Day -> TZ -> Item -> H.Html
viewItem today _ (DayHeading day) =
    H.h2 ! A.class_ "upcomingDayHeading" $ do
        H.toHtml $ Time.formatTime
            Time.defaultTimeLocale
            "%a %e %b %Y"
            day
        when (day == today) " (Today)"
        when (day == succ today) " (Tomorrow)"
-- TODO: we should group events in a day into list elements.
viewItem _ targetZone (Occurrence Oc.Occurrence{Oc.ocItem, Oc.ocTimeStamp = zot}) =
    let title = eventSummary $ DB.eeVEvent ocItem
        timeStamp = case Oc.ocTimeInZoneFudge targetZone zot of
            Oc.LocalOCAllDay _ ->
                H.span ! A.class_ "eventTime" $ "All Day"
            Oc.LocalOCAtTime Time.LocalTime{Time.localTimeOfDay} ->
                H.time ! A.class_ "eventTime"
                    ! A.datetime (H.toValue $ show localTimeOfDay)
                    $ H.toHtml $ Time.formatTime
                        Time.defaultTimeLocale
                        "%l:%M %p"
                        localTimeOfDay
    in
    H.div ! A.class_ "upcomingEvent" $ do
        H.p $ timeStamp
        H.p $ H.a
            ! A.href (H.toValue $ Route.Event (DB.eeId ocItem) (Just zot))
            $ title

home :: [LT.Text] -> Time.Day -> TZ -> [Oc.Occurrence DB.EventEntry] -> H.Html
home permissions today targetZone entries = docToHtml Document
    { permissions
    , title = "Upcoming Events"
    , body = do
        H.h1 "Upcoming Events"
        traverse_ (viewItem today targetZone) (makeItems entries)
    }
