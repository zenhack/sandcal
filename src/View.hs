{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module View
    ( editEvent
    , View.Event.event
    , View.Home.home
    , View.NewEvent.newEvent
    , View.Week.week
    ) where

import Zhp

import qualified Forms.NewEvent as NewEvent
import qualified Route
import qualified Util.TZ        as TZ
import qualified View.EditEvent as EditEvent
import qualified View.Event
import qualified View.Home
import qualified View.NewEvent
import qualified View.Week

import qualified Util.CSRF as CSRF

editEvent csrfKey userId userTz eid ev =
    let tzLabel = case userTz of
            Just v  -> v
            Nothing -> TZ.Etc__UTC
        route = Route.PostEditEvent eid
    in
    EditEvent.editEvent EditEvent.EditTemplate
        { title = "Edit Event"
        , submitText = "Update"
        , action = Route.PostEditEvent eid
        , userTz
        , formData = Just $ NewEvent.fromVEvent tzLabel ev
        , csrfToken = CSRF.makeCsrfToken csrfKey (CSRF.PostCap route userId)
        }
