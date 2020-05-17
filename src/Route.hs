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

data Route
    = Get GetRoute
    | Post PostRoute

data GetRoute
    = Home
    | Settings
    | Event Int64
    | NewEvent
    | StyleCss

data PostRoute
    = PostNewEvent
    | SaveSettings
    | ImportICS

instance ToValue Route where
    toValue (Get r)  = toValue r
    toValue (Post r) = toValue r

instance ToValue PostRoute where
    toValue PostNewEvent = "/event/new"
    toValue SaveSettings = "/settings"
    toValue ImportICS    = "/import.ics"

instance ToValue GetRoute where
    toValue = renderGet

renderGet :: IsString a => GetRoute -> a
renderGet Home        = "/"
renderGet Settings    = "/settings"
renderGet (Event eid) = fromString $ "/event/" <> show eid
renderGet NewEvent    = "/event/new"
renderGet StyleCss    = "/style.css"

redirectGet :: GetRoute -> ActionM ()
redirectGet rt = do
    setHeader "Location" (renderGet rt)
    status status303

scottyM :: (Route -> ActionM ()) -> ScottyM ()
scottyM route = do
    get "/" $
        route $ Get Home
    get "/settings" $
        route $ Get Settings
    get "/event/new" $
        route $ Get NewEvent
    get "/event/:eid" $ do
        eid <- param "eid"
        route $ Get $ Event eid
    get "/style.css" $
        route $ Get StyleCss
    post "/event/new" $
        route $ Post PostNewEvent
    post "/settings" $
        route $ Post SaveSettings
    post "/import.ics" $
        route $ Post ImportICS
