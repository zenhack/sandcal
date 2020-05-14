{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Data.Text.Encoding.Error      (lenientDecode)
import Data.Time.Zones.All           (TZLabel, fromTZName, toTZName)
import Network.HTTP.Types.Status     (status400, status404)
import SandCal.Config                (cfgDBPath, getConfig)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              (Html)
import Text.ICalendar.Parser         (parseICalendar)

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Default            as Default
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Route
import qualified SandCal.DB              as DB
import qualified Sandstorm
import qualified View


import Web.Scotty
import Zhp

blaze :: Html -> ActionM ()
blaze = html . renderHtml

encodeTZLabel :: TZLabel -> LT.Text
encodeTZLabel =
    toTZName
    >>> LBS.fromStrict
    >>> LT.decodeUtf8With lenientDecode

decodeTZLabel :: LT.Text -> Maybe TZLabel
decodeTZLabel = LT.encodeUtf8 >>> LBS.toStrict >>> fromTZName

main :: IO ()
main = do
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.open dbPath
    DB.runQuery db DB.initSchema
    scotty 3000 $ do
        Route.scottyM $ \case
            Route.Get Route.StyleCss -> file "style.css"

            Route.Get Route.Home -> viewHome db
            Route.Get Route.Settings -> viewSettings db
            Route.Get Route.NewEvent -> viewNewEvent db
            Route.Get _ -> error "TODO"
            Route.Post Route.SaveSettings -> setTimeZone db
            Route.Post Route.PostNewEvent -> postNewEvent db
        get "/api/event/:eid" $ do
            eid <- param "eid"
            getEvent db (DB.eventID eid)
        get "/api/timezone" $ getTimeZone db
        get "/settings" $ viewSettings db
        post "/api/timezone" $ setTimeZone db
        post "/api/import.ics" $ importICS db
        notFound $ do404

elmPage = do
    setHeader "Content-Type" "text/html"
    file "index.html"

viewNewEvent _db =
    blaze View.newEvent

viewHome db = do
    events <- DB.runQuery db DB.allEvents
    blaze $ View.home events

getEvent db eid = do
    res <- DB.runQuery db (DB.getEvent eid)
    case res of
        Nothing -> do404
        Just e  -> json e

importICS db = do
    bytes <- body
    case parseICalendar Default.def "import.ics" bytes of
        Left err -> do
            liftIO $ putStrLn $ "Error parsing icalendar data: " <> err
            status status400
            text "Invalid icalendar file."

        Right (vcals, warns) -> do
            liftIO $ for_ warns $ \warning ->
                putStrLn $ "Warning (parsing icalendar data): " <> warning
            traverse_ (DB.runQuery db . DB.addCalendar) vcals

viewSettings db = do
    uid <- Sandstorm.getUserId
    result <- DB.runQuery db $ View.settings uid
    blaze result

getTimeZone db = do
    uid <- Sandstorm.getUserId
    timezone <- liftIO $ DB.runQuery db $ DB.getUserTimeZone uid
    case timezone of
        Just tz -> json (encodeTZLabel tz)
        Nothing -> do
            status status404
            text "No timezone set."

postNewEvent _db = do
    summary <- param "Summary"
    date <- param "Date"
    startTime <- param "Start Time"
    endTime <- param "End Time"
    error $ show (summary :: String, date :: String, startTime :: String, endTime :: String)

setTimeZone db = do
    uid <- Sandstorm.getUserId
    timezone <- param "timezone"
    case decodeTZLabel timezone of
        Just tzLabel -> do
            liftIO $ DB.runQuery db $ DB.setUserTimeZone uid tzLabel
            Route.redirectGet Route.Home
        Nothing -> do
            status status400
            text "Invalid time zone."

do404 = do
    status status404
    elmPage
