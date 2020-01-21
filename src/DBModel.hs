{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
module DBModel
    ( connect
    , initSchema
    , withDB
    , Event(..)
    , allEvents
    ) where

import Database.Selda
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Database.Selda.SQLite
import Zhp

import Config

connect :: FilePath -> IO (SeldaConnection SQLite)
connect = sqliteOpen

withDB :: (MonadIO m, MonadMask m) => SeldaConnection SQLite -> SeldaT SQLite m a -> m a
withDB db m = runSeldaT m db

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
