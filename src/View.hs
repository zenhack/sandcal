{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module View
    ( settings
    , newEvent
    , event
    , View.Home.home
    ) where

import Zhp

import View.Common

import qualified Data.Text.Lazy              as LT
import qualified ICal
import qualified Route
import qualified SandCal.DB                  as DB
import qualified Sandstorm                   as Sandstorm
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified View.Home

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

newEvent :: H.Html
newEvent = docToHtml Document
    { title = "New Event"
    , body = postForm mempty Route.PostNewEvent $ do
        labeledInput "Summary" mempty
        labeledInput "Date" $ A.type_ "date"
        labeledInput "Start Time" $ A.type_ "time"
        labeledInput "End Time" $ A.type_ "time"
        H.button ! A.type_ "submit" $ "Create Event"
    }

settings :: Sandstorm.UserId -> DB.Query H.Html
settings uid =
    flip fmap (DB.getUserTimeZone uid) $ \userTz ->
        docToHtml $ Document
            { title = "User Settings"
            , body =
                postForm mempty Route.SaveSettings $ do
                    tzSelect "timezone" userTz
                    H.button ! A.type_ "submit" $ "Save Settings"
            }
