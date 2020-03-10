{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module SandCal.DB
    ( Conn
    , Query
    , open
    , runQuery

    , eventID

    , EventEntry(..)

    , initSchema
    , allEvents
    , addEvent
    , addCalendar
    , getEvent
    , getUserTimeZone
    , setUserTimeZone
    ) where

import Zhp

import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as DB
import qualified ICal

import Database.SQLite.Simple (NamedParam((:=)))

import GHC.Generics (Generic)
import Text.Heredoc (here)

----- wrappers, to abstract out storage details.

newtype Conn = Conn DB.Connection

newtype ID a = ID Int64

instance Show (ID a) where
    show (ID x) = show x

eventID :: Int64 -> ID ICal.VEvent
eventID = ID

newtype Query a = Query { getQueryFn :: DB.Connection -> IO a }

open :: MonadIO m => String -> m Conn
open path = Conn <$> liftIO (DB.open path)

runQuery :: MonadIO m => Conn -> Query a -> m a
runQuery (Conn conn) (Query f) = liftIO $ f conn


---- Helper types that appear in our queries


data EventEntry = EventEntry
    { eeId     :: !Int64
    , eeVEvent :: !ICal.VEvent
    }
    deriving(Generic)
instance Aeson.ToJSON EventEntry
instance Aeson.FromJSON EventEntry

instance DB.FromRow EventEntry where
    fromRow = do
        (eeId, vevent) <- DB.fromRow
        let !ev = mustDecode vevent
        pure EventEntry { eeId, eeVEvent = ev }

mustDecode :: BS.ByteString -> ICal.VEvent
mustDecode bytes = case Aeson.decode (LBS.fromStrict bytes) of
    Just ev -> ev
    Nothing -> error "Failed to decode vevent in the db; corrupted database?"

----- canned queries.

-- | Initialize the schema.
initSchema :: Query ()
initSchema = Query $ \conn -> do
    DB.execute_ conn
        [here|
            CREATE TABLE IF NOT EXISTS events (
                id INTEGER PRIMARY KEY,
                vevent BLOB NOT NULL
            )
        |]
    DB.execute_ conn
        [here|
            CREATE TABLE IF NOT EXISTS user_timezones (
                user_id VARCHAR PRIMARY KEY,
                timezone_name VARCHAR NOT NULL
            )
        |]

allEvents :: Query [EventEntry]
allEvents = Query $ \conn -> DB.query_ conn "SELECT id, vevent FROM events"

addEvent :: ICal.VEvent -> Query (ID ICal.VEvent)
addEvent ev = Query $ \conn -> do
    DB.executeNamed conn
        "INSERT INTO events(vevent) VALUES(:event)"
        [":event" := Aeson.encode ev]
    ID <$> DB.lastInsertRowId conn

addCalendar :: ICal.VCalendar -> Query ()
addCalendar vcal = Query $ \conn ->
    DB.withTransaction conn $ do
        for_ (ICal.vcEvents vcal) $ \ev ->
            getQueryFn (addEvent ev) conn

getEvent :: ID ICal.VEvent -> Query (Maybe ICal.VEvent)
getEvent (ID ident) = Query $ \conn -> do
    rs <- DB.queryNamed conn "SELECT vevent FROM events WHERE id = :ident" [":ident" := ident]
    case rs of
        []            -> pure Nothing
        (DB.Only r:_) -> pure $! Just $! mustDecode r

--- TODO: use wrapper types instead of timezones.

setUserTimeZone :: T.Text -> T.Text -> Query ()
setUserTimeZone userId timezoneName = Query $ \conn -> do
    DB.executeNamed conn
        [here|
            INSERT INTO user_timezones(user_id, timezone_name)
            VALUES (:user_id, :timeone_name)
        |]
        [ ":user_id" := userId
        , ":timezone_name" := timezoneName
        ]


getUserTimeZone :: T.Text -> Query (Maybe T.Text)
getUserTimeZone userId = Query $ \conn -> do
    rs <- DB.queryNamed conn
        "SELECT timezone_name FROM user_timezones WHERE user_id = :user_id"
        [":user_id" := userId]
    case rs of
        []            -> pure Nothing
        (DB.Only r:_) -> pure $! Just $! r
