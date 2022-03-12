{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module View.NewEvent
  ( newEvent,
  )
where

import qualified Data.Text.Lazy as LT
import qualified Route
import qualified Sandstorm
import qualified Text.Blaze.Html5 as H
import qualified Util.CSRF as CSRF
import View.EditEvent
import Zhp

newEvent :: CSRF.Key -> [LT.Text] -> Maybe Sandstorm.UserId -> H.Html
newEvent csrfKey permissions uid =
  let route = Route.PostNewEvent
   in editEvent
        permissions
        EditTemplate
          { title = "New Event",
            submitText = "Create",
            action = route,
            userTz = Nothing,
            formData = Nothing,
            csrfToken = CSRF.makeCsrfToken csrfKey (CSRF.PostCap route uid)
          }
