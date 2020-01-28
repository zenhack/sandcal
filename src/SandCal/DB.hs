{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
module SandCal.DB
    ( DB
    , DBT
    , connect
    , initSchema
    , with
    , Event(..)
    , Recur(..)
    , allEvents
    , addEvent
    , getEvent
    , addRecur
    ) where

import Database.Selda         hiding (with)
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Database.Selda.SQLite
import Zhp

import qualified Data.Text as T
import           Text.Read (readMaybe)

import qualified ICal.Types as ICal

type DB = SeldaConnection SQLite

type DBT m a = SeldaT SQLite m a

connect :: FilePath -> IO DB
connect = sqliteOpen

with :: (MonadIO m, MonadMask m) => DB -> DBT m a -> m a
with db m = runSeldaT m db

-- | Initialize the database.
initSchema :: MonadSelda m => m ()
initSchema = do
    createTable events
    createTable recurs

-------------------- Schema -----------------------

data Event = Event
    { evId      :: ID Event
    , evSummary :: Text
    , evDTStart :: Int
    } deriving(Show, Generic)
instance SqlRow Event

data Recur = Recur
    { rEventId   :: ID Event
    , rFrequency :: ICal.Frequency
    , rUntil     :: Maybe Int
    } deriving(Show, Generic)
instance SqlRow Recur

events :: Table Event
events = table "events" [#evId :- autoPrimary]

recurs :: Table Recur
recurs = table "recurs" []

-------------------- Canned queries -----------------------

allEvents :: MonadSelda m => m [Event]
allEvents = query $ select events

addEvent :: MonadSelda m => Event -> m (ID Event)
addEvent ev = insertWithPK events [ev]

addRecur :: MonadSelda m => Recur -> m (ID Recur)
addRecur r = insertWithPK recurs [r]

getEvent :: MonadSelda m => T.Text -> m (Maybe (Event, [Recur]))
getEvent identTxt =
    case readMaybe (T.unpack identTxt) of
        Nothing       -> pure Nothing
        Just identInt -> go (toId identInt)
  where
    go ident = do
        es <- query $ do
            event <- select events
            restrict (event ! #evId .== literal ident)
            pure event
        case es of
            [] -> pure Nothing
            (_:_:_) -> error "impossible: duplicate event ids"
            [event] -> do
                rs <- query $ do
                    recur <- select recurs
                    restrict $ recur ! #rEventId .== literal (evId event)
                    pure recur
                pure $ Just (event, rs)
