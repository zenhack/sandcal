{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Zhp

import Database.Selda                (def)
import Text.Blaze.Html               (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Heredoc                  (there)

import Web.Scotty

import qualified SandCal.ApiTypes as ApiTypes
import           SandCal.Config   (cfgDBPath, getConfig)
import qualified SandCal.DB       as DB
import           SandCal.Route    (Method(..), Route(..))
import qualified SandCal.Route    as Route
import qualified SandCal.View     as View

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
    ev <- jsonData
    eid <- liftIO $ DB.with db $ DB.addEvent DB.Event
        { DB.evId = def
        , DB.evSummary = ApiTypes.summary ev
        , DB.evDTStart = ApiTypes.start ev
        }
    json (show eid)
