{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
module DBModel
    ( initDB
    ) where

import Database.Selda
import Zhp

-- | Initialize the database.
initDB :: MonadSelda m => m ()
initDB = do
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
