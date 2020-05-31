{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module View
    ( settings
    , event
    , View.Home.home
    , View.NewEvent.newEvent
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
import qualified View.NewEvent

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
