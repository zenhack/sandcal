{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module View.Event
  ( event,
  )
where

import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import qualified FindLinks
import qualified Occurrences as Oc
import qualified Route
import qualified Sandstorm
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Util.CSRF as CSRF
import qualified Util.ICal as ICal
import qualified Util.TZ as TZ
import Util.Time (addICalDuration)
import View.Common
import Zhp

event :: CSRF.Key -> [LT.Text] -> Maybe Sandstorm.UserId -> Int64 -> TZ.TZLabel -> ICal.VEvent -> Maybe Oc.ZonedOCTime -> H.Html
event csrfKey permissions userId eid tzLabel ev zot =
  let title = case ICal.veSummary ev of
        Nothing -> "Untitled Event"
        Just ICal.Summary {ICal.summaryValue} -> summaryValue
      Just zot' =
        zot <|> (fmap Oc.ocTimeStamp (Oc.firstOccurrence tzLabel ev))
   in docToHtml
        Document
          { permissions,
            title = "Event - " <> LT.toStrict title,
            body = do
              H.h1 $ H.toHtml title
              when ("editor" `elem` permissions) $ do
                H.nav $
                  H.ul $ do
                    H.li $ H.a ! A.href (H.toValue $ Route.Get $ Route.EditEvent eid) $ "Edit"
                    H.li $
                      postLink csrfKey (CSRF.PostCap (Route.PostDeleteEvent eid) userId) $
                        "Delete (all occurrences)"
              {- TODO: implement the handler for this and then uncomment.
                                      for_ zot $ \z ->
                                          H.li $ postLink (Route.PostDeleteOccurrence eid z) $ "Delete (this occurrence only)"
              -}
              let locTime = Oc.ocTimeInZoneFudge (TZ.tzByLabel tzLabel) zot'
              viewTime locTime (ICal.veventDuration ev)
              for_ (ICal.veDescription ev) $ \de -> do
                H.h2 "Description"
                viewDescription $ ICal.descriptionValue de
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
                    H.li $ markupLinks name
              for_ (ICal.veLocation ev) $ \loc -> do
                H.h2 "Location"
                markupLinks $ ICal.locationValue loc
          }

viewRRule :: ICal.RRule -> H.Html
viewRRule rr =
  let recur = ICal.rRuleValue rr
      freq = ICal.freqUnitName (ICal.recurFreq recur)
   in H.toHtml $ case ICal.recurInterval recur of
        1 -> "every " <> freq
        interval -> "every " <> show interval <> " " <> freq <> "s"

viewTime :: Oc.LocalOCTime -> Maybe ICal.Duration -> H.Html
viewTime locTime dur = case locTime of
  Oc.LocalOCAllDay day -> do
    H.toHtml $
      Time.formatTime
        Time.defaultTimeLocale
        "%a %e %b %Y"
        day
  Oc.LocalOCAtTime localTime ->
    let start =
          Time.formatTime
            Time.defaultTimeLocale
            "%a %e %b %Y, %l:%M %p"
            localTime
     in case dur of
          Nothing -> H.toHtml start
          Just d ->
            let localTime' = addICalDuration d localTime
                end =
                  Time.formatTime
                    Time.defaultTimeLocale
                    ( if Time.localDay localTime == Time.localDay localTime'
                        then "%l:%M %p"
                        else "%a %e %b %Y, %l:%M %p"
                    )
                    localTime'
             in H.toHtml $ start <> " - " <> end

viewDescription :: LT.Text -> H.Html
viewDescription descr =
  H.div ! A.class_ "eventDescription" $ markupLinks descr

markupLinks :: LT.Text -> H.Html
markupLinks =
  LT.lines
    >>> map (FindLinks.renderWithLinks linkRenderer)
    >>> intersperse H.br
    >>> mconcat

linkRenderer :: FindLinks.Renderer H.Html
linkRenderer =
  FindLinks.Renderer
    { FindLinks.renderText = H.toHtml,
      FindLinks.renderLink = \url text ->
        H.a ! A.href (H.toValue url)
          ! A.target "_blank"
          ! A.rel "noreferrer noopener"
          $ H.toHtml text
    }
