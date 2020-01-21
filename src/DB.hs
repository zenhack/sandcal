{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
module DB
    ( DB
    , DBT
    , connect
    , initSchema
    , with
    , Event(..)
    , allEvents
    ) where

import Database.Selda         hiding (with)
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Database.Selda.SQLite
import Zhp

import Config

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

-------------------- Schema -----------------------

data Event = Event
    { evId      :: ID Event
    , evSummary :: Text
    , evDTStart :: Int
    } deriving(Show, Generic)
instance SqlRow Event

events :: Table Event
events = table "events" [#evId :- autoPrimary]

-------------------- Canned queries -----------------------

allEvents :: MonadSelda m => m [Event]
allEvents = query $ select events
