{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module View.NewEvent
    ( newEvent
    ) where

import Zhp

import View.EditEvent

import qualified Route
import qualified Text.Blaze.Html5 as H

import qualified Sandstorm
import qualified Util.CSRF as CSRF

newEvent :: CSRF.Key -> Maybe Sandstorm.UserId -> H.Html
newEvent csrfKey uid =
    let route = Route.PostNewEvent in
    editEvent EditTemplate
        { title = "New Event"
        , submitText = "Create"
        , action = route
        , userTz = Nothing
        , formData = Nothing
        , csrfToken = CSRF.makeCsrfToken csrfKey (CSRF.PostCap route uid)
        }
