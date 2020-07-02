{-# LANGUAGE NamedFieldPuns #-}
module View.Event
    ( event
    ) where

import Zhp

import View.Common

import qualified Data.Set         as S
import qualified Data.Text.Lazy   as LT
import qualified ICal
import qualified Text.Blaze.Html5 as H

event :: ICal.VEvent -> H.Html
event ev =
    let title = case ICal.veSummary ev of
            Nothing                               -> "Untitled Event"
            Just ICal.Summary {ICal.summaryValue} -> summaryValue
    in
    docToHtml Document
        { title = "Event - " <> LT.toStrict title
        , body = do
            H.h1 $ H.toHtml title
            for_ (ICal.veDescription ev) $ \de ->
                -- TODO: try to be smarter about formatting, e.g. insert paragraphs
                -- and such instead of just throwing a <pre> at it.
                H.pre $ H.toHtml $ ICal.descriptionValue de
            let participants = ICal.veAttendee ev
            unless (S.null participants) $ do
                H.h2 "Participants"
                for_ participants $ \p -> H.ul $ do
                    for_ (ICal.attendeeCN p) $ \name ->
                        H.li $ maybeLink name (ICal.attendeeDir p)
            for_ (ICal.veLocation ev) $ \loc -> do
                H.h2 "Location"
                H.p $ H.toHtml $ ICal.locationValue loc
        }
