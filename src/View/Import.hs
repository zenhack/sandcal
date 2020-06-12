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
            formBlock $
                labeledInput "Calendar File" $ A.type_ "file" <> A.accept "text/calendar"
            H.button ! A.type_ "submit" $ "Upload"
    }
