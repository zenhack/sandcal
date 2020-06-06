module Route
    ( Route(..)
    , GetRoute(..)
    , PostRoute(..)
    , scottyM
    , redirectGet
    ) where

import Zhp

import Network.HTTP.Types.Status (status303)
import Text.Blaze                (ToValue(toValue))
import Web.Scotty

import qualified Data.Time as Time

data Route
    = Get GetRoute
    | Post PostRoute

data GetRoute
    = Home
    | Week Time.Day
    | Settings
    | Event Int64
    | NewEvent
    | StyleCss
    | ImportICS

data PostRoute
    = PostNewEvent
    | SaveSettings
    | PostImportICS

instance ToValue Route where
    toValue (Get r)  = toValue r
    toValue (Post r) = toValue r

instance ToValue PostRoute where
    toValue PostNewEvent  = "/event/new"
    toValue SaveSettings  = "/settings"
    toValue PostImportICS = "/import.ics"

instance ToValue GetRoute where
    toValue = renderGet

renderGet :: IsString a => GetRoute -> a
renderGet Home        = "/"
renderGet (Week day) =
    let (y, m, d) = Time.toGregorian day in
    fromString $ "/week/" <> show y <> "/" <> show m <> "/" <> show d
renderGet Settings    = "/settings"
renderGet (Event eid) = fromString $ "/event/" <> show eid
renderGet NewEvent    = "/event/new"
renderGet StyleCss    = "/style.css"
renderGet ImportICS   = "/import"

redirectGet :: GetRoute -> ActionM ()
redirectGet rt = do
    setHeader "Location" (renderGet rt)
    status status303

scottyM :: (Route -> ActionM ()) -> ScottyM ()
scottyM route = do
    get "/" $
        route $ Get Home
    get "/week/:y/:m/:d" $ do
        y <- param "y"
        m <- param "m"
        d <- param "d"
        route $ Get $ Week $ Time.fromGregorian y m d
    get "/settings" $
        route $ Get Settings
    get "/event/new" $
        route $ Get NewEvent
    get "/event/:eid" $ do
        eid <- param "eid"
        route $ Get $ Event eid
    get "/style.css" $
        route $ Get StyleCss
    get "/import" $
        route $ Get ImportICS
    post "/event/new" $
        route $ Post PostNewEvent
    post "/settings" $
        route $ Post SaveSettings
    post "/import.ics" $
        route $ Post PostImportICS
