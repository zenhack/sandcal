{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module DB
  ( Conn,
    Query,
    open,
    runQuery,
    ID,
    eventID,
    unEventID,
    EventEntry (..),
    initSchema,
    allEvents,
    addEvent,
    addCalendar,
    getEvent,
    updateEvent,
    deleteEvent,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as Sql
import GHC.Generics (Generic)
import Text.Heredoc (here)
import qualified Util.ICal as ICal
import Zhp

----- wrappers, to abstract out storage details.

newtype Conn = Conn Sql.Connection

newtype ID a = ID Int64

instance Show (ID a) where
  show (ID x) = show x

eventID :: Int64 -> ID ICal.VEvent
eventID = ID

unEventID :: ID ICal.VEvent -> Int64
unEventID (ID x) = x

newtype Query a = Query {getQueryFn :: Sql.Connection -> IO a}

instance Functor Query where
  fmap f q = Query $ fmap f . getQueryFn q

instance Applicative Query where
  pure x = Query $ \_ -> pure x
  f <*> x = Query $ \conn -> getQueryFn f conn <*> getQueryFn x conn

instance Monad Query where
  x >>= f = Query $ \conn -> do
    x' <- getQueryFn x conn
    getQueryFn (f x') conn

open :: MonadIO m => String -> m Conn
open path = Conn <$> liftIO (Sql.open path)

runQuery :: MonadIO m => Conn -> Query a -> m a
runQuery (Conn conn) (Query f) =
  liftIO $ Sql.withTransaction conn $ f conn

---- Helper types that appear in our queries

data EventEntry = EventEntry
  { eeId :: !Int64,
    eeVEvent :: !ICal.VEvent
  }
  deriving (Show, Generic)

instance Aeson.ToJSON EventEntry

instance Aeson.FromJSON EventEntry

instance Sql.FromRow EventEntry where
  fromRow = do
    (eeId, vevent) <- Sql.fromRow
    let !ev = mustDecode vevent
    pure EventEntry {eeId, eeVEvent = ev}

mustDecode :: BS.ByteString -> ICal.VEvent
mustDecode bytes = case Aeson.decode (LBS.fromStrict bytes) of
  Just ev -> ev
  Nothing -> error "Failed to decode vevent in the db; corrupted database?"

----- canned queries.

-- | Initialize the schema.
initSchema :: Query ()
initSchema = Query $ \conn -> do
  Sql.execute_
    conn
    [here|
            CREATE TABLE IF NOT EXISTS events (
                id INTEGER PRIMARY KEY,
                vevent BLOB NOT NULL
            )
        |]

allEvents :: Query [EventEntry]
allEvents = Query $ \conn -> Sql.query_ conn "SELECT id, vevent FROM events"

addEvent :: ICal.VEvent -> Query (ID ICal.VEvent)
addEvent ev = Query $ \conn -> do
  Sql.executeNamed
    conn
    "INSERT INTO events(vevent) VALUES(:event)"
    [":event" := Aeson.encode ev]
  ID <$> Sql.lastInsertRowId conn

addCalendar :: ICal.VCalendar -> Query ()
addCalendar vcal = traverse_ addEvent (ICal.vcEvents vcal)

getEvent :: ID ICal.VEvent -> Query (Maybe ICal.VEvent)
getEvent (ID ident) = Query $ \conn -> do
  rs <- Sql.queryNamed conn "SELECT vevent FROM events WHERE id = :ident" [":ident" := ident]
  case rs of
    [] -> pure Nothing
    (Sql.Only r : _) -> pure $! Just $! mustDecode r

updateEvent :: ID ICal.VEvent -> (ICal.VEvent -> ICal.VEvent) -> Query (Maybe ())
updateEvent eid f = do
  maybeEv <- getEvent eid
  for maybeEv $ \ev ->
    Query $ \conn ->
      Sql.executeNamed
        conn
        "UPDATE events SET vevent = :event WHERE id = :ident"
        [ ":event" := Aeson.encode (f ev),
          ":ident" := unEventID eid
        ]

deleteEvent :: ID ICal.VEvent -> Query ()
deleteEvent eid = Query $ \conn ->
  Sql.executeNamed
    conn
    "DELETE FROM events WHERE id = :ident"
    [":ident" := unEventID eid]
