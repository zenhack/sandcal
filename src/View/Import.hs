module View.Import
    ( importICS
    ) where

import Zhp

import View.Common

import qualified Route

import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

importICS = docToHtml $ Document
    { title = "Import Calendar"
    , body = do
        H.h1 "Import Calendar"
        postForm (A.enctype "multipart/form-data") Route.PostImportICS $ do
            H.p $ mconcat
                [ "Import calendar events from an .ics file. Note that this functionality "
                , "is alpha quality; SandCal may not interpret all ics data correctly."
                ]
            formBlock $
                labeledInput "Calendar File" $ A.type_ "file" <> A.accept "text/calendar"
            H.button ! A.type_ "submit" $ "Upload"
    }
