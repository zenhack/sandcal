{-# LANGUAGE NamedFieldPuns #-}
module View.Home
    ( home
    ) where

import Zhp

import View.Common

import qualified Occurrences as Oc
import qualified Route
import qualified SandCal.DB  as DB

import qualified Data.Time                   as Time
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

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

viewItem :: Item -> H.Html
viewItem (DayHeading day) =
    H.h2 ! A.class_ "upcomingDayHeading" $
        H.toHtml $ Time.formatTime
            Time.defaultTimeLocale
            "%a %e %b %Y"
            day
-- TODO: we should group events in a day into list elements.
viewItem (Occurrence Oc.Occurrence{Oc.ocItem, Oc.ocTimeStamp = zot}) =
    let title = eventSummary $ DB.eeVEvent ocItem
        timeStamp = case Oc.octTime zot of
            Oc.LocalOCAllDay _ ->
                "All Day"
            Oc.LocalOCAtTime Time.LocalTime{Time.localTimeOfDay} ->
                H.toHtml $ show localTimeOfDay
    in
    H.div ! A.class_ "upcomingEvent" $ do
        timeStamp
        " : "
        H.a
            ! A.href (H.toValue $ Route.Event $ DB.eeId ocItem)
            $ title

home :: [Oc.Occurrence DB.EventEntry] -> H.Html
home entries = docToHtml Document
    { title = "Upcoming Events"
    , body = do
        H.h1 "Upcoming Events"
        H.ul $ traverse_ viewItem (makeItems entries)
    }
