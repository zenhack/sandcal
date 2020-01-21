{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Zhp

import qualified Data.Text.Lazy as LT

import Database.Selda                (def)
import Text.Blaze.Html               (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Heredoc                  (there)

import Web.Scotty

import Config (cfgDBPath, getConfig)

import qualified ApiTypes
import qualified DB
import qualified Forms
import           Route    (Method(..), Route(..))
import qualified Route
import qualified View

blaze :: Html -> ActionM ()
blaze = html . renderHtml

indexHtml = [there|ui/index.html|]

main :: IO ()
main = do
    args <- getArgs
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.connect dbPath
    when (args == ["--init"]) $ do
        DB.with db DB.initSchema
    scotty 3000 $ traverse_ (handle db)
        [ Root
        , AllEvents
        , NewEvent GET
        , NewEvent POST
        ]

handle db rt = do
    let method = case Route.method rt of
            GET  -> get
            POST -> post
    method (Route.path rt) $ handleRt db rt

handleRt :: DB.DB -> Route -> ActionM ()
handleRt _db Root = do
    html indexHtml
handleRt db AllEvents = do
    events <- liftIO $ DB.with db DB.allEvents
    json $
        [ ApiTypes.Event
            { ApiTypes.summary = DB.evSummary e
            , ApiTypes.start = DB.evDTStart e
            , ApiTypes.end = DB.evDTStart e -- TODO: actually add the end field to the db.
            }
        | e <- events
        ]
handleRt _db (NewEvent GET) =
    blaze $ View.page "Sandcal - New Event" (pure ()) View.newEventForm
handleRt db (NewEvent POST) = do
    f <- Forms.parseForm
    eid <- liftIO $ DB.with db $ DB.addEvent DB.Event
        { DB.evId = def
        , DB.evSummary = LT.toStrict $ Forms.neSummary f
        , DB.evDTStart = Forms.neStart f
        }
    text (fromString $ show eid)
