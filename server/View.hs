{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module View
    ( settings
    ) where

import Zhp

import qualified Data.ByteString.Char8       as B8
import qualified Data.Text                   as T
import qualified Data.Time.Zones.DB          as Tz
import qualified SandCal.DB                  as DB
import qualified Sandstorm                   as Sandstorm
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Document = Document
    { title :: T.Text
    , body  :: H.Html
    }

docToHtml :: Document -> H.Html
docToHtml Document{title, body} = H.docTypeHtml $ do
    H.title $ H.toHtml title
    H.body $ body

settings :: Sandstorm.UserId -> DB.Query H.Html
settings uid =
    flip fmap (DB.getUserTimeZone uid) $ \userTz ->
        let addSelected :: Tz.TZLabel -> (H.Html -> H.Html) -> H.Html -> H.Html
            addSelected tz elt
             | (Just tz) == userTz = elt ! A.selected ""
             | otherwise = elt

            name :: IsString a => Tz.TZLabel -> a
            name tz = fromString $ B8.unpack $ Tz.toTZName tz

            tzOption :: Tz.TZLabel -> H.Html
            tzOption tz = addSelected tz H.option ! A.value (name tz) $ (name tz)
        in
        docToHtml $ Document
            { title = "User Settings"
            , body =
                H.form $ do
                    H.select $ traverse_ tzOption [minBound..maxBound]
                    H.button ! A.type_ "submit" $ "Save Settings"
            }
