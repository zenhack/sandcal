module View.NewEvent
    ( newEvent
    ) where

import Zhp

import View.Common

import qualified Route

import qualified Data.Time.Zones.All         as Tz
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

newEvent :: Maybe Tz.TZLabel -> H.Html
newEvent userTz = docToHtml Document
    { title = "New Event"
    , body = do
        H.h1 $ "New Event"
        postForm mempty Route.PostNewEvent $ do
            labeledInput "Summary" mempty
            labeledInput "Date" $ A.type_ "date"
            labeledInput "Start Time" $ A.type_ "time"
            labeledInput "End Time" $ A.type_ "time"
            labeledTzSelect "Time Zone" userTz
            H.button ! A.type_ "submit" $ "Create Event"
    }
