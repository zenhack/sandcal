{-# LANGUAGE DuplicateRecordFields #-}
module View.EditEvent
    ( EditTemplate(..)
    , editEvent
    ) where

import Zhp

import qualified View.Common as VC

import qualified Route

import qualified Data.ByteString.Char8       as BS8
import qualified Data.Text                   as T
import qualified Data.Time.Zones.All         as Tz
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data EditTemplate = EditTemplate
    { title  :: T.Text
    , userTz :: Maybe Tz.TZLabel
    , action :: Route.PostRoute
    }

editEvent :: EditTemplate -> H.Html
editEvent tpl = VC.docToHtml VC.Document
    { title = title tpl
    , body = do
        H.script ! A.src "/bundle.min.js" $ pure ()
        H.h1 $ H.toHtml $ title tpl
        H.div
            ! A.id "bs-form"
            ! H.dataAttribute "sandcal-action" (H.toValue $ Route.Post $ action tpl)
            ! H.dataAttribute "sandcal-user-tz" (case userTz tpl of
                Nothing -> ""
                Just tz -> H.toValue (BS8.unpack $ Tz.toTZName tz)
              )
            $ pure ()
    }
