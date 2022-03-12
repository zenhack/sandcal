module Route
  ( Route (..),
    GetRoute (..),
    PostRoute (..),
    scottyM,
    redirectGet,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Time as Time
import Network.HTTP.Types.Status (status303)
import qualified Network.URI as URI
import qualified Occurrences as Oc
import Text.Blaze (ToValue (toValue))
import Text.Read (readMaybe)
import Web.Scotty
import Zhp

data Route
  = Get GetRoute
  | Post PostRoute
  deriving (Show)

data GetRoute
  = Home
  | Week Time.Day
  | Event Int64 (Maybe Oc.ZonedOCTime)
  | NewEvent
  | EditEvent Int64
  | StyleCss
  | SandstormJS
  | ImportICS
  | ExportICS
  deriving (Show)

data PostRoute
  = PostNewEvent
  | PostEditEvent Int64
  | PostImportICS
  | PostDeleteEvent Int64
  | PostDeleteOccurrence Int64 Oc.ZonedOCTime
  deriving (Show)

instance ToValue Route where
  toValue (Get r) = toValue r
  toValue (Post r) = toValue r

instance ToValue PostRoute where
  toValue = renderPost

instance ToValue GetRoute where
  toValue = renderGet

instance Aeson.FromJSON PostRoute where
  parseJSON = error "TODO"

instance Aeson.ToJSON PostRoute where
  toJSON = Aeson.String . renderPost

renderPost :: IsString a => PostRoute -> a
renderPost (PostEditEvent eid) = fromString $ "/event/" <> show eid <> "/edit"
renderPost PostNewEvent = "/event/new"
renderPost PostImportICS = "/import.ics"
renderPost (PostDeleteEvent eid) = fromString $ "/event/" <> show eid <> "/delete"
renderPost (PostDeleteOccurrence eid zot) =
  fromString $
    mconcat
      [ "/event/",
        show eid,
        "/delete?occurrence=",
        URI.escapeURIString URI.isAllowedInURI (show zot)
      ]

renderGet :: IsString a => GetRoute -> a
renderGet Home = "/"
renderGet (Week day) =
  let (y, m, d) = Time.toGregorian day
   in fromString $ "/week/" <> show y <> "/" <> show m <> "/" <> show d
renderGet (Event eid zot) =
  fromString $
    mconcat
      [ "/event/",
        show eid,
        case zot of
          Nothing -> ""
          Just z ->
            mconcat
              [ "?occurrence=",
                URI.escapeURIString URI.isAllowedInURI (show z)
              ]
      ]
renderGet (EditEvent eid) = fromString $ "/event/" <> show eid <> "/edit"
renderGet NewEvent = "/event/new"
renderGet StyleCss = "/style.css"
renderGet SandstormJS = "/sandstorm.js"
renderGet ImportICS = "/import"
renderGet ExportICS = "/export.ics"

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
  get "/event/new" $
    route $ Get NewEvent
  get "/event/:eid" $ do
    eid <- param "eid"
    occurStr <- (fmap Just (param "occurrence")) `rescue` (const $ pure Nothing)
    route $ Get $ Event eid (occurStr >>= readMaybe . URI.unEscapeString)
  get "/event/:eid/edit" $ do
    eid <- param "eid"
    route $ Get $ EditEvent eid
  post "/event/:eid/edit" $ do
    eid <- param "eid"
    route $ Post $ PostEditEvent eid
  get "/style.css" $
    route $ Get StyleCss
  get "/sandstorm.js" $
    route $ Get SandstormJS
  get "/import" $
    route $ Get ImportICS
  get "/export.ics" $
    route $ Get ExportICS
  post "/event/new" $
    route $ Post PostNewEvent
  post "/import.ics" $
    route $ Post PostImportICS
  post "/event/:eid/delete" $ do
    eid <- param "eid"
    occurStr <- (fmap Just (param "occurrence")) `rescue` (const $ pure Nothing)
    case occurStr >>= (readMaybe . URI.unEscapeString) of
      Nothing -> route $ Post $ PostDeleteEvent eid
      Just occur -> route $ Post $ PostDeleteOccurrence eid occur
