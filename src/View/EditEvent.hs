{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module View.EditEvent
  ( EditTemplate (..),
    editEvent,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Forms.NewEvent (NewEvent)
import GHC.Generics (Generic)
import qualified Route
import Text.Blaze (ToValue (toValue))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Util.TZ as TZ
import qualified View.Common as VC
import Zhp

data EditTemplate = EditTemplate
  { title :: T.Text,
    submitText :: T.Text,
    userTz :: Maybe TZ.TZLabel,
    action :: Route.PostRoute,
    formData :: Maybe NewEvent,
    csrfToken :: String
  }
  deriving (Show, Generic)

instance Aeson.ToJSON EditTemplate

instance Aeson.FromJSON EditTemplate

instance ToValue EditTemplate where
  toValue =
    Aeson.encode
      >>> LT.decodeUtf8With lenientDecode
      >>> LT.unpack
      >>> fromString

editEvent :: [LT.Text] -> EditTemplate -> H.Html
editEvent permissions tpl =
  VC.docToHtml
    VC.Document
      { permissions,
        title = title tpl,
        body = do
          H.script ! A.src "/bundle.min.js" $ pure ()
          H.h1 $ H.toHtml $ title tpl
          H.div
            ! A.id "bs-form"
            ! H.dataAttribute "sandcal-template" (H.toValue tpl)
            $ pure ()
      }
