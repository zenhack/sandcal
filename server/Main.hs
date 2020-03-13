{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Zhp

import qualified Data.Default as Default

import Network.HTTP.Types.Status (status400, status404)
import Text.Read                 (readMaybe)
import Web.Scotty

import           SandCal.Config (cfgDBPath, getConfig)
import qualified SandCal.DB     as DB

import qualified Sandstorm

import Text.ICalendar.Parser (parseICalendar)

main :: IO ()
main = do
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.open dbPath
    DB.runQuery db DB.initSchema
    scotty 3000 $ do
        get "/" elmPage
        get "/ui.js" $ do
            setHeader "Content-Type" "application/javascript"
            file "ui.js"
        get "/event/:eid" elmPage
        get "/event/new" elmPage
        get "/api/all-events.json" $ getAllEvents db
        post "/api/event/new" $ postNewEvent db
        get "/api/event/:eid" $ do
            eid <- param "eid"
            getEvent db (DB.eventID eid)
        get "/api/timezone" $ getTimeZone db
        post "/api/timezone" $ setTimeZone db
        post "/api/import.ics" $ importICS db
        notFound $ do404

elmPage = do
    setHeader "Content-Type" "text/html"
    file "index.html"

getAllEvents db = do
    events <- DB.runQuery db DB.allEvents
    json events

getEvent db eid = do
    res <- DB.runQuery db (DB.getEvent eid)
    case res of
        Nothing -> do404
        Just e  -> json e

postNewEvent db = do
    ev <- jsonData
    eid <- DB.runQuery db (DB.addEvent ev)
    json (show eid)

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

getTimeZone db = do
    uid <- Sandstorm.getUserId
    timezone <- liftIO $ DB.runQuery db $ DB.getUserTimeZone uid
    case timezone of
        Just tz -> json (show tz)
        Nothing -> do
            status status404
            text "No timezone set."

setTimeZone db = do
    uid <- Sandstorm.getUserId
    reqBody <- jsonData
    case readMaybe reqBody of
        Just tzLabel ->
            liftIO $ DB.runQuery db $ DB.setUserTimeZone uid tzLabel
        Nothing -> do
            status status400
            text "Invalid time zone."

do404 = do
    status status404
    elmPage
