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

import qualified Route
import qualified SandCal.DB                  as DB
import qualified Sandstorm                   as Sandstorm
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified View.EditEvent              as EditEvent
import qualified View.Event
import qualified View.Home
import qualified View.NewEvent
import qualified View.Week

settings :: Sandstorm.UserId -> DB.Query H.Html
settings uid =
    flip fmap (DB.getUserTimeZone uid) $ \userTz ->
        docToHtml $ Document
            { title = "User Settings"
            , body = do
                H.h1 "User Settings"
                postForm mempty Route.SaveSettings $ do
                    formBlock $
                        labeledTzSelect "Time Zone" userTz
                    H.button ! A.type_ "submit" $ "Save"
            }

editEvent userTz eid = EditEvent.editEvent EditEvent.EditTemplate
    { title = "Edit Event"
    , submitText = "Update"
    , action = Route.PostEditEvent eid
    , userTz
    , formData = Nothing -- TODO: actually fill in the data.
    }
