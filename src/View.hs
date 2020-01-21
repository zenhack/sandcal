module View
    ( page
    , events
    ) where

import Zhp hiding (div)

import qualified Data.Text.Lazy              as LT
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import DB (Event(..))

page :: LT.Text -> Html -> Html -> Html
page titleText headExtra bodyContent =
    docTypeHtml $
        html $ do
            head $ do
                meta ! A.charset "utf-8"
                title $ toHtml titleText
                headExtra
            body $
                bodyContent

event :: Event -> Html
event ev = div $ do
    toHtml (evSummary ev)
    -- TODO: format this as a proper date/time.
    time ! A.class_ "dt-start" $ toHtml (show (evDTStart ev))

events :: [Event] -> Html
events evs =
    ul $ for_ evs $ \ev ->
        li $ event ev
