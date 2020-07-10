{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module View.Event
    ( event
    ) where

import Zhp

import View.Common

import qualified Data.Set                    as S
import qualified Data.Text.Lazy              as LT
import qualified Data.Time                   as Time
import qualified Data.Time.Zones.All         as TZ
import qualified ICal
import qualified Occurrences                 as Oc
import qualified Route
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

event :: Int64 -> TZ.TZLabel -> ICal.VEvent -> Maybe Oc.ZonedOCTime -> H.Html
event eid tzLabel ev zot =
    let title = case ICal.veSummary ev of
            Nothing                               -> "Untitled Event"
            Just ICal.Summary {ICal.summaryValue} -> summaryValue
        Just zot' =
            zot <|> (fmap Oc.ocTimeStamp (Oc.firstOccurrence tzLabel ev))
    in
    docToHtml Document
        { title = "Event - " <> LT.toStrict title
        , body = do
            H.h1 $ H.toHtml title
            H.nav $ H.ul $
                H.li $ H.a ! A.href (H.toValue $ Route.Get $ Route.EditEvent eid) $ "Edit"
            H.toHtml $ viewLocalOCTime (Oc.octTime zot')
            for_ (ICal.veDescription ev) $ \de -> do
                H.h2 "Description"
                -- TODO: try to be smarter about formatting, e.g. insert paragraphs
                -- and such instead of just throwing a <pre> at it.
                H.pre $ H.toHtml $ ICal.descriptionValue de
            let rrules = ICal.veRRule ev
            case S.size rrules of
                0 -> pure ()
                1 -> do
                    H.p $ do
                        "Repeats "
                        for_ rrules viewRRule
                _ -> do
                    H.h2 "Repeats"
                    H.ul $ do
                        for_ rrules $ \r ->
                            H.li $ viewRRule r
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

viewRRule :: ICal.RRule -> H.Html
viewRRule rr =
    let recur = ICal.rRuleValue rr
        freq = show (ICal.recurFreq recur)
            & map toLower
            -- Chop the -ly off the end:
            & reverse
            & drop 2
            & reverse
    in
    H.toHtml $ case ICal.recurInterval recur of
        1        -> "every " <> freq
        interval -> "every " <> show interval <> " " <> freq <> "s"

viewLocalOCTime :: Oc.LocalOCTime-> H.Html
viewLocalOCTime = \case
    Oc.LocalOCAllDay day ->
        H.toHtml $ Time.formatTime
            Time.defaultTimeLocale
            "%a %e %b %Y"
            day
    Oc.LocalOCAtTime localTime ->
        H.toHtml $ Time.formatTime
            Time.defaultTimeLocale
            "%a %e %b %Y, %l:%M %p"
            localTime
