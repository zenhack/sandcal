{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module View.EditEvent
    ( EditTemplate(..)
    , editEvent
    ) where

import Zhp

import qualified View.Common as VC

import qualified Route

import TZ ()

import qualified Data.Aeson                  as Aeson
import qualified Data.Text                   as T
import           Data.Text.Encoding.Error    (lenientDecode)
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LT
import qualified Data.Time.Zones.All         as Tz
import           Forms.NewEvent              (NewEvent)
import           GHC.Generics                (Generic)
import           Text.Blaze                  (ToValue(toValue))
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data EditTemplate = EditTemplate
    { title      :: T.Text
    , submitText :: T.Text
    , userTz     :: Maybe Tz.TZLabel
    , action     :: Route.PostRoute
    , formData   :: Maybe NewEvent
    }
    deriving(Show, Generic)
instance Aeson.ToJSON EditTemplate
instance Aeson.FromJSON EditTemplate

instance ToValue EditTemplate where
    toValue = Aeson.encode
        >>> LT.decodeUtf8With lenientDecode
        >>> LT.unpack
        >>> fromString

editEvent :: EditTemplate -> H.Html
editEvent tpl = VC.docToHtml VC.Document
    { title = title tpl
    , body = do
        H.script ! A.src "/bundle.min.js" $ pure ()
        H.h1 $ H.toHtml $ title tpl
        H.div
            ! A.id "bs-form"
            ! H.dataAttribute "sandcal-template" (H.toValue tpl)
            $ pure ()
    }
