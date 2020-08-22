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

import qualified CSRF
import qualified Sandstorm

newEvent :: CSRF.Key -> Sandstorm.UserId -> Maybe Tz.TZLabel -> H.Html
newEvent csrfKey uid userTz =
    let route = Route.PostNewEvent in
    editEvent EditTemplate
        { title = "New Event"
        , submitText = "Create"
        , action = route
        , userTz
        , formData = Nothing
        , csrfToken = CSRF.makeCsrfToken csrfKey (CSRF.PostCap route uid)
        }
