module View.Home
    ( home
    ) where

import Zhp

import View.Common

import qualified ICal
import qualified Occurrences as Oc
import qualified Route
import qualified SandCal.DB  as DB

import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

home :: [Oc.Occurrence DB.EventEntry] -> H.Html
home entries = docToHtml Document
    { title = "All Events"
    , body = do
        H.a ! A.href (H.toValue $ Route.NewEvent) $ "New Event"
        H.h1 "Import Calendar"
        postForm (A.enctype "multipart/form-data") Route.ImportICS $ do
            labeledInput "Calendar File" $ A.type_ "file" <> A.accept "text/calendar"
            H.button ! A.type_ "submit" $ "Upload"
        H.h1 "All Events"
        H.ul $ for_ entries $ \oc ->
            let ee = Oc.ocItem oc in
            H.li $ H.a
                ! A.href (H.toValue $ Route.Event $ DB.eeId ee)
                $ case ICal.veSummary $ DB.eeVEvent ee of
                    Just summary -> H.toHtml $ ICal.summaryValue summary
                    Nothing      -> "Untitled event"
    }
