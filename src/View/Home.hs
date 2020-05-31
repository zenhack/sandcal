{-# LANGUAGE NamedFieldPuns #-}
module View.Home
    ( home
    ) where

import Zhp

import View.Common

import qualified ICal
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
    H.h2 $ H.toHtml (show day) -- TODO: nicer formatting
viewItem (Occurrence Oc.Occurrence{Oc.ocItem, Oc.ocTimeStamp = zot}) =
    let title = case ICal.veSummary $ DB.eeVEvent ocItem of
            Just summary -> H.toHtml $ ICal.summaryValue summary
            Nothing      -> "Untitled event"
        timeStamp = case Oc.octTime zot of
            Oc.LocalOCAllDay _ ->
                "All Day"
            Oc.LocalOCAtTime Time.LocalTime{Time.localTimeOfDay} ->
                H.toHtml $ show localTimeOfDay
    in
    H.div $ do
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
