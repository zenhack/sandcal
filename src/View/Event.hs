{-# LANGUAGE NamedFieldPuns #-}
module View.Event
    ( event
    ) where

import Zhp

import View.Common

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
        , body =
            H.h1 $ H.toHtml title
        }
