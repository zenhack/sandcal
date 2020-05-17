{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Data.Default                  (def)
import Data.Text.Encoding.Error      (lenientDecode)
import Data.Time.Zones.All           (TZLabel, fromTZName, toTZName)
import Network.HTTP.Types.Status     (status400, status404)
import Network.Wai.Parse             (FileInfo(..))
import SandCal.Config                (cfgDBPath, getConfig)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              (Html)
import Text.ICalendar.Parser         (parseICalendar)

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Time               as Time
import qualified Data.Time.Zones         as Tz
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import qualified ICal
import qualified Route
import qualified SandCal.DB              as DB
import qualified Sandstorm
import qualified View

import qualified DateParsers as DP


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
            Route.Get (Route.Event eid) -> getEvent db eid
            Route.Post Route.SaveSettings -> setTimeZone db
            Route.Post Route.PostNewEvent -> postNewEvent db
            Route.Post Route.ImportICS -> importICS db
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
    res <- DB.runQuery db (DB.getEvent (DB.eventID eid))
    case res of
        Nothing -> do404
        Just e  -> blaze $ View.event e

importICS db = do
    fs <- files
    let cals = for fs $ \(_, FileInfo{fileName, fileContent}) ->
            parseICalendar def (show fileName) fileContent
    case cals of
        Left err -> do
            liftIO $ do
                putStrLn $ "Error parsing icalendar data: " <> err
                hFlush stdout
            status status400
            text "Invalid icalendar file."

        Right cals' -> do
            liftIO $ do
                for_ cals' $ \(_, warns) ->
                    for_ warns $ \warning ->
                        putStrLn $ "Warning (parsing icalendar data): " <> warning
                print cals'
                hFlush stdout
            DB.runQuery db $
                for_ cals' $ \(vcals, _) ->
                    traverse_ DB.addCalendar vcals
            Route.redirectGet Route.Home

viewSettings db = do
    uid <- Sandstorm.getUserId
    result <- DB.runQuery db $ View.settings uid
    blaze result

postNewEvent db = do
    summary <- param "Summary"
    DP.Day day <- param "Date"
    DP.TimeOfDay startTime <- param "Start Time"
    DP.TimeOfDay endTime <- param "End Time"

    uid <- Sandstorm.getUserId
    maybeTzLabel <- DB.runQuery db $ DB.getUserTimeZone uid
    maybeTz <- liftIO $ for maybeTzLabel $
        encodeTZLabel
        >>> LT.unpack
        >>> Tz.loadSystemTZ

    uuid <- liftIO $ UUID.nextRandom

    let floatingStart = Time.LocalTime
            { Time.localDay = day
            , Time.localTimeOfDay = startTime
            }
        floatingEnd = floatingStart
            { Time.localTimeOfDay = endTime
            }
        dtStart = case maybeTzLabel of
            Nothing -> ICal.FloatingDateTime
                { ICal.dateTimeFloating = floatingStart
                }
            Just tz -> ICal.ZonedDateTime
                { ICal.dateTimeFloating = floatingStart
                , ICal.dateTimeZone = encodeTZLabel tz
                }
        dtEnd = dtStart { ICal.dateTimeFloating = floatingEnd }

        !utcStart = case maybeTz of
            Just tz -> Tz.localTimeToUTCTZ tz floatingStart
            Nothing -> error $ mconcat
                [ "TODO: deal with the case where the user doesn't give us "
                , "a time zone."
                ]

        vEvent = ICal.VEvent
            { ICal.veUID = ICal.UID
                { ICal.uidValue = LT.fromStrict $ UUID.toText uuid
                , ICal.uidOther = def
                }
            , ICal.veDTStamp = ICal.DTStamp
                { ICal.dtStampValue = utcStart
                , ICal.dtStampOther = def
                }
            , ICal.veSummary = Just ICal.Summary
                { ICal.summaryValue = summary
                , ICal.summaryAltRep = def
                , ICal.summaryLanguage = def
                , ICal.summaryOther = def
                }
            , ICal.veDTStart = Just ICal.DTStartDateTime
                { ICal.dtStartDateTimeValue = dtStart
                , ICal.dtStartOther = def
                }
            , ICal.veDTEndDuration = Just $ Left ICal.DTEndDateTime
                { ICal.dtEndDateTimeValue = dtEnd
                , ICal.dtEndOther = def
                }

            -- TODO: fill these in with the current time:
            , ICal.veCreated = Nothing
            , ICal.veLastMod = Nothing

            -- Not used for now:
            , ICal.veClass = def
            , ICal.veDescription = def
            , ICal.veGeo = def
            , ICal.veLocation = def
            , ICal.veOrganizer = def
            , ICal.vePriority = def
            , ICal.veSeq = def
            , ICal.veStatus = def
            , ICal.veTransp = def
            , ICal.veUrl = def
            , ICal.veRecurId = def
            , ICal.veRRule = def
            , ICal.veAttach = def
            , ICal.veAttendee = def
            , ICal.veCategories = def
            , ICal.veComment = def
            , ICal.veContact = def
            , ICal.veExDate = def
            , ICal.veRStatus  = def
            , ICal.veRelated = def
            , ICal.veResources = def
            , ICal.veRDate = def
            , ICal.veAlarms = def
            , ICal.veOther = def
            }
    evId <- DB.runQuery db $ DB.addEvent vEvent
    Route.redirectGet $ Route.Event $ DB.unEventID evId

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
