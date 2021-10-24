{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module View.Common
    ( Document(..)
    , docToHtml
    , labeledInput
    , navigation
    , postForm
    , postLink
    , formBlock
    , tzSelect
    , labeledTzSelect
    , labeledSelect
    , eventSummary
    ) where

import Zhp

import qualified Route
import qualified Util.CSRF as CSRF

import qualified Data.ByteString.Char8       as B8
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Util.ICal                   as ICal
import qualified Util.TZ                     as TZ

data Document = Document
    { title       :: T.Text
    , permissions :: [LT.Text]
    , body        :: H.Html
    }

eventSummary :: ICal.VEvent -> H.Html
eventSummary ev = case ICal.veSummary ev of
    Just summary -> H.toHtml $ ICal.summaryValue summary
    Nothing      -> "Untitled event"

docToHtml :: Document -> H.Html
docToHtml Document{title, body, permissions} = H.docTypeHtml $ do
    H.title $ H.toHtml (title <> " · SandCal")
    H.link ! A.rel "stylesheet" ! A.href (H.toValue Route.StyleCss)
    H.script ! A.src (H.toValue Route.SandstormJS) $ pure ()
    H.body $ do
        navigation permissions
        H.div ! A.class_ "mainContentContainer" $
            H.div ! A.class_ "mainContent" $ body

labeledInput :: T.Text -> H.Attribute -> H.Html
labeledInput name attrs =
    let name' = H.toValue name in
    H.div ! A.class_ "labeledInput" $ do
        H.label ! A.for name' $ H.toHtml name
        H.input ! A.name name' ! attrs

postForm :: CSRF.Key -> CSRF.PostCap -> H.Attribute -> H.Html -> H.Html
postForm csrfKey cap attrs contents =
    let token = CSRF.makeCsrfToken csrfKey cap in
    H.form
        ! attrs
        ! A.method "post"
        ! A.action (H.toValue (CSRF.route cap)) $ do
            H.input ! A.type_ "hidden" ! A.name "csrfToken" ! A.value (H.toValue token)
            contents

postLink :: CSRF.Key -> CSRF.PostCap -> String -> H.Html
postLink  key cap label =
    postForm key cap (A.class_ "postLink") $
        H.button ! A.type_ "submit" $ H.toHtml label

formBlock :: H.Html -> H.Html
formBlock body = do
    H.div
        ! A.class_ "formBlock"
        $ body

tzSelect :: T.Text -> Maybe TZ.TZLabel -> H.Html
tzSelect label userTz =
    let addSelected :: TZ.TZLabel -> (H.Html -> H.Html) -> H.Html -> H.Html
        addSelected tz elt
         | (Just tz) == userTz = elt ! A.selected ""
         | otherwise = elt

        name :: IsString a => TZ.TZLabel -> a
        name tz = fromString $ B8.unpack $ TZ.toTZName tz

        tzOption :: TZ.TZLabel -> H.Html
        tzOption tz = addSelected tz H.option ! A.value (name tz) $ (name tz)
    in
    H.select ! A.id (H.toValue label) ! A.name (H.toValue label) $
        traverse_ tzOption [minBound..maxBound]

labeledTzSelect :: T.Text -> Maybe TZ.TZLabel -> H.Html
labeledTzSelect name userTz =
    H.div ! A.class_ "labeledInput" $ do
        H.label ! A.for (H.toValue name) $ H.toHtml name
        tzSelect name userTz

labeledSelect :: (H.ToMarkup a, H.ToValue a) => T.Text -> [(a, Bool)] -> H.Html
labeledSelect selectName options =
    H.div ! A.class_ "labeledInput" $ do
        let selectNameVal = H.toValue selectName
        H.label ! A.for selectNameVal $ H.toHtml selectName
        H.select ! A.id selectNameVal ! A.name selectNameVal $ do
            for_ options $ \(name, selected) ->
                let opt' = H.option ! A.value (H.toValue name)
                    opt'' = if selected
                        then opt' ! A.selected ""
                        else opt'
                in
                opt'' $ H.toHtml name

navigation :: [LT.Text] -> H.Html
navigation permissions =
    H.nav $ H.ul $ traverse_ navItem items
  where
    navItem (rt, label) =
        H.li $ H.a ! A.href (H.toValue rt) $ label
    items =
        if "editor" `elem` permissions then
            [ (Route.Home, "Upcoming Events")
            , (Route.NewEvent, "New Event")
            , (Route.ImportICS, "Import/Export")
            ]
        else
            [ (Route.Home, "Upcoming Events")
            , (Route.ImportICS, "Export")
            ]
