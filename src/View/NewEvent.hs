module View.NewEvent
    ( newEvent
    ) where

import Zhp

import View.Common

import qualified Data.ByteString.Char8       as BS8
import qualified Data.Time.Zones.All         as Tz
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

newEvent :: Maybe Tz.TZLabel -> H.Html
newEvent userTz = docToHtml Document
    { title = "New Event"
    , body = do
        H.script ! A.src "/bundle.min.js" $ pure ()
        H.h1 $ "New Event"
        H.div
            ! A.id "bs-form"
            ! H.dataAttribute "sandcal-form-id" "new-event"
            ! H.dataAttribute "sandcal-user-tz" (case userTz of
                Nothing -> ""
                Just tz -> H.toValue (BS8.unpack $ Tz.toTZName tz)
              )
            $ pure ()
    }
