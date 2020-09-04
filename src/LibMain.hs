{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
module LibMain (main) where

import Config                        (cfgDBPath, getConfig)
import Data.Default                  (def)
import Network.HTTP.Types.Status     (status400, status404)
import Network.Wai.Parse             (FileInfo(..))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              (Html)
import Text.ICalendar.Parser         (parseICalendar)


import qualified Forms.NewEvent
import qualified Forms.Settings

import Util.TZ (TZLabel)

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Time               as Time
import qualified Data.UUID.V4            as UUID
import qualified DB
import qualified Occurrences
import qualified Route
import qualified Sandstorm
import qualified Util.CSRF               as CSRF
import qualified Util.Scotty.Cookie      as Cookie
import qualified Util.Time               as UT
import qualified Util.TZ                 as TZ
import qualified View
import qualified View.Import


import Web.Scotty
import Zhp

blaze :: Html -> ActionM ()
blaze = html . renderHtml

main :: IO ()
main = do
    csrfKey <- CSRF.genKey
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.open dbPath
    DB.runQuery db DB.initSchema
    scotty 3000 $ do
        Route.scottyM $ \case
            Route.Get Route.StyleCss -> file "style.css"
            Route.Get Route.SandstormJS -> file "sandstorm.js"

            Route.Get Route.Home -> viewHome db
            Route.Get (Route.Week refDay) -> viewWeek db refDay
            Route.Get Route.Settings -> viewSettings csrfKey db
            Route.Get Route.NewEvent -> viewNewEvent csrfKey db
            Route.Get (Route.Event eid zot) -> getEvent csrfKey db eid zot
            Route.Get (Route.EditEvent eid) -> editEvent csrfKey db eid
            Route.Get Route.ImportICS -> do
                maybeUid <- Sandstorm.maybeGetUserId
                blaze $ View.Import.importICS csrfKey maybeUid

            Route.Post postRt -> do
                CSRF.verifyPostRoute csrfKey postRt
                case postRt of
                    Route.PostEditEvent eid -> postEditEvent db eid
                    Route.PostNewEvent -> postNewEvent db
                    Route.PostImportICS -> importICS db

                    Route.SaveSettings -> do
                        uid <- Sandstorm.getUserId
                        Forms.Settings.Settings{timeZone} <- Forms.Settings.getForm
                        liftIO $ DB.runQuery db $ DB.setUserTimeZone uid timeZone
                        Route.redirectGet Route.Home

                    Route.PostDeleteEvent eid -> do
                        liftIO $ DB.runQuery db $ DB.deleteEvent (DB.eventID eid)
                        Route.redirectGet Route.Home

                    Route.PostDeleteOccurrence eid zot -> do
                        _ <- liftIO $ DB.runQuery db $ deleteOccurrence (DB.eventID eid) zot
                        Route.redirectGet Route.Home

        get "/bundle.min.js" $ file "ui/bundle.min.js"
        notFound $ do404

viewNewEvent csrfKey db = do
    uid <- Sandstorm.maybeGetUserId
    maybeTzLabel <- case uid of
        Just uid' -> DB.runQuery db $ DB.getUserTimeZone uid'
        Nothing   -> pure Nothing
    blaze $ View.newEvent csrfKey uid maybeTzLabel

occursBefore :: Occurrences.Occurrence a -> Time.UTCTime -> Bool
occursBefore occur end =
    Occurrences.zonedOCTimeToUTCFudge (Occurrences.ocTimeStamp occur) <= end

viewHome db = do
    uid <- Sandstorm.maybeGetUserId
    utcNow <- liftIO $ Time.getCurrentTime
    let utcEnd = Time.addUTCTime (Time.nominalDay * 45) utcNow
    occurs <- getOccursSince db utcNow
        & fmap (takeWhile (`occursBefore` utcEnd))
        & fmap (take 20)
    tzLabel <- userTZOrUTC db
    blaze $ View.home uid (TZ.tzByLabel tzLabel) occurs

viewWeek db refDay = do
    -- TODO: allow the user to configure the start of the week.
    let firstDayOfWeek = Time.Sunday
        (startDay, endDay) = UT.weekBounds firstDayOfWeek refDay
    uid <- Sandstorm.maybeGetUserId
    tzLabel <- userTZOrUTC db
    let tz = TZ.tzByLabel tzLabel
        utcStart = TZ.localTimeToUTCTZ tz (UT.startOfDay startDay)
        utcEnd   = TZ.localTimeToUTCTZ tz (UT.endOfDay endDay)
        zonedOCTime = Occurrences.ZonedOCTime
            { octZone = tzLabel
            , octTime = Occurrences.LocalOCAllDay refDay
            }
    occurs <- takeWhile (`occursBefore` utcEnd)
        <$> getOccursSince db utcStart
    blaze $ View.week uid firstDayOfWeek zonedOCTime occurs

getOccursSince :: DB.Conn -> Time.UTCTime -> ActionM [Occurrences.Occurrence DB.EventEntry]
getOccursSince db utc = do
    events <- DB.runQuery db DB.allEvents
    tzLabel <- userTZOrUTC db
    pure $ events
        & map (\ev ->
            Occurrences.eventOccurrences tzLabel utc (DB.eeVEvent ev)
            & map (fmap (\vEv -> ev { DB.eeVEvent = vEv }))
        )
        & Occurrences.merge

getUserTZ :: DB.Conn -> ActionM (Maybe TZLabel)
getUserTZ db = do
    cookieTz <- Cookie.getCookie "timezone"
    maybeUid <- Sandstorm.maybeGetUserId
    maybeTz <- case maybeUid of
        Nothing  -> pure Nothing
        Just uid -> DB.runQuery db $ DB.getUserTimeZone uid
    pure $ asum
        [ maybeTz
        , do
            tz <- cookieTz
            TZ.fromTZName (LBS.toStrict $ LTE.encodeUtf8 tz)
        ]

userTZOrUTC :: DB.Conn -> ActionM TZLabel
userTZOrUTC db = do
    maybeTz <- getUserTZ db
    pure $ case maybeTz of
        Just tz -> tz
        Nothing -> TZ.Etc__UTC

eventOr404 db eid = do
    res <- DB.runQuery db (DB.getEvent (DB.eventID eid))
    case res of
        Nothing -> do404
        Just e  -> pure e

getEvent csrfKey db eid zot = do
    maybeUid <- Sandstorm.maybeGetUserId
    e <- eventOr404 db eid
    tzLabel <- userTZOrUTC db
    blaze $ View.event csrfKey maybeUid eid tzLabel e zot

editEvent csrfToken db eid = do
    uid <- Sandstorm.maybeGetUserId
    userTz <- getUserTZ db
    event <- eventOr404 db eid
    blaze $ View.editEvent csrfToken uid userTz eid event

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
            DB.runQuery db $
                for_ cals' $ \(vcals, _) ->
                    traverse_ DB.addCalendar vcals
            Route.redirectGet Route.Home

viewSettings csrfKey db = do
    uid <- Sandstorm.getUserId
    result <- DB.runQuery db $ View.settings csrfKey uid
    blaze result

postNewEvent db = do
    utcNow <- liftIO $ Time.getCurrentTime
    uuid <- liftIO $ UUID.nextRandom
    form <- Forms.NewEvent.getForm
    let vEvent = Forms.NewEvent.toVEvent utcNow uuid form
    evId <- DB.runQuery db $ DB.addEvent vEvent
    Route.redirectGet $ Route.Event (DB.unEventID evId) Nothing

postEditEvent db eid = do
    utcNow <- liftIO $ Time.getCurrentTime
    form <- Forms.NewEvent.getForm
    maybeEv <- DB.runQuery db $
        DB.updateEvent (DB.eventID eid) $ Forms.NewEvent.patchVEvent utcNow form
    case maybeEv of
        Nothing -> do404
        Just () -> Route.redirectGet $ Route.Event eid Nothing

deleteOccurrence eid _zot = DB.updateEvent eid $ \ev -> ev -- TODO

do404 = do
    status status404
    text "404 - not found"
    finish
