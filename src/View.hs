{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module View
    ( settings
    , home
    , newEvent
    ) where

import Zhp

import qualified Data.ByteString.Char8       as B8
import qualified Data.Text                   as T
import qualified Data.Time.Zones.DB          as Tz
import qualified ICal
import qualified Route
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
    H.link ! A.rel "stylesheet" ! A.href (H.toValue Route.StyleCss)
    H.body $ body


-- FIXME(security): xsrf.
postForm :: Route.PostRoute -> H.Html -> H.Html
postForm rt contents =
    H.form ! A.class_ "postForm" ! A.method "post" ! A.action (H.toValue rt) $
        contents

home :: [DB.EventEntry] -> H.Html
home entries = docToHtml Document
    { title = "All Events"
    , body = do
        H.h1 "All Events"
        H.ul $ for_ entries $ \ee ->
            H.li $ H.a
                ! A.href (H.toValue $ Route.Event $ DB.eeId ee)
                $ case ICal.veSummary $ DB.eeVEvent ee of
                    Just summary -> H.toHtml $ ICal.summaryValue summary
                    Nothing      -> "Untitled event"
        H.a ! A.href (H.toValue $ Route.NewEvent) $ "New Event"
{-
            [ button
                [ onClick SelectFile ]
                [ text "Import ICalendar File" ]
            ]
-}
    }

newEvent :: H.Html
newEvent = docToHtml Document
    { title = "New Event"
    , body = postForm Route.PostNewEvent $ do
        labeledInput "Summary" mempty
        labeledInput "Date" $ A.type_ "date"
        labeledInput "Start Time" $ A.type_ "time"
        labeledInput "End Time" $ A.type_ "time"
        H.button ! A.type_ "submit" $ "Create Event"
    }

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
                postForm Route.SaveSettings $ do
                    H.select ! A.name "timezone" $ traverse_ tzOption [minBound..maxBound]
                    H.button ! A.type_ "submit" $ "Save Settings"
            }

labeledInput :: T.Text -> H.Attribute -> H.Html
labeledInput name attrs =
    let name' = H.toValue name in
    H.div ! A.class_ "labeledInput" $ do
        H.label ! A.for name' $ H.toHtml name
        H.input ! A.name name' ! attrs