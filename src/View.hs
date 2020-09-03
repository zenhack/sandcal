{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module View
    ( settings
    , editEvent
    , View.Event.event
    , View.Home.home
    , View.NewEvent.newEvent
    , View.Week.week
    ) where

import Zhp

import View.Common

import qualified Data.Time.Zones.All         as Tz
import qualified DB
import qualified Forms.NewEvent              as NewEvent
import qualified Route
import qualified Sandstorm                   as Sandstorm
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified View.EditEvent              as EditEvent
import qualified View.Event
import qualified View.Home
import qualified View.NewEvent
import qualified View.Week

import qualified CSRF

settings :: CSRF.Key -> Sandstorm.UserId -> DB.Query H.Html
settings csrfKey uid =
    flip fmap (DB.getUserTimeZone uid) $ \userTz ->
        docToHtml $ Document
            { user = Just uid
            , title = "User Settings"
            , body = do
                H.h1 "User Settings"
                postForm csrfKey (CSRF.PostCap Route.SaveSettings (Just uid)) mempty $ do
                    formBlock $
                        labeledTzSelect "Time Zone" userTz
                    H.button ! A.type_ "submit" $ "Save"
            }

editEvent csrfKey userId userTz eid ev =
    let tzLabel = case userTz of
            Just v  -> v
            Nothing -> Tz.Etc__UTC
        route = Route.PostEditEvent eid
    in
    EditEvent.editEvent userId EditEvent.EditTemplate
        { title = "Edit Event"
        , submitText = "Update"
        , action = Route.PostEditEvent eid
        , userTz
        , formData = Just $ NewEvent.fromVEvent tzLabel ev
        , csrfToken = CSRF.makeCsrfToken csrfKey (CSRF.PostCap route userId)
        }
