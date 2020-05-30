{-# LANGUAGE NamedFieldPuns #-}
module View.Common
    ( Document(..)
    , docToHtml
    , labeledInput
    , postForm
    ) where

import Zhp

import qualified Route

import qualified Data.Text                   as T
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

labeledInput :: T.Text -> H.Attribute -> H.Html
labeledInput name attrs =
    let name' = H.toValue name in
    H.div ! A.class_ "labeledInput" $ do
        H.label ! A.for name' $ H.toHtml name
        H.input ! A.name name' ! attrs

-- FIXME(security): xsrf.
postForm :: H.Attribute -> Route.PostRoute -> H.Html -> H.Html
postForm attrs rt contents =
    H.form
        ! attrs
        ! A.class_ "postForm"
        ! A.method "post"
        ! A.action (H.toValue rt) $
            contents
