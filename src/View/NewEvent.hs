{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module View.NewEvent
    ( newEvent
    ) where

import Zhp

import View.EditEvent

import qualified Data.Time.Zones.All as Tz
import qualified Route
import qualified Text.Blaze.Html5    as H

newEvent :: Maybe Tz.TZLabel -> H.Html
newEvent userTz = editEvent EditTemplate
    { title = "New Event"
    , submitText = "Create"
    , action = Route.PostNewEvent
    , userTz
    }
