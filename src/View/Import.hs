{-# LANGUAGE NamedFieldPuns #-}

module View.Import
  ( importICS,
  )
where

import qualified Route
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Util.CSRF as CSRF
import View.Common
import Zhp

importICS csrfKey permissions userId =
  docToHtml $
    Document
      { permissions,
        title =
          if "editor" `elem` permissions
            then "Import/Export Calendar"
            else "Export Calendar",
        body = do
          when ("editor" `elem` permissions) $ do
            H.h1 "Import Calendar"
            postForm
              csrfKey
              (CSRF.PostCap Route.PostImportICS userId)
              (A.enctype "multipart/form-data")
              $ do
                H.p $
                  mconcat
                    [ "Import calendar events from an .ics file. Note that this functionality ",
                      "is alpha quality; SandCal may not interpret all ics data correctly."
                    ]
                formBlock $
                  labeledInput "Calendar File" $
                    A.type_ "file" <> A.accept "text/calendar"
                H.button ! A.type_ "submit" $ "Upload"
          H.h1 "Export Calendar"
          H.a ! (A.href $ H.toValue Route.ExportICS) $ "Download"
          H.div $ do
            H.label "Subscribe: "
            H.iframe ! (A.id "export-offer-iframe") $ pure ()
      }
