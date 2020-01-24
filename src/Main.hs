{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Zhp

import Database.Selda (def)

import Network.HTTP.Types.Status (status404)
import Web.Scotty

import qualified SandCal.ApiTypes as ApiTypes
import           SandCal.Config   (cfgDBPath, getConfig)
import qualified SandCal.DB       as DB
import           SandCal.Route    (Method(..), Route(..))
import qualified SandCal.Route    as Route

elmPage = do
    setHeader "Content-Type" "text/html"
    file "index.html"

main :: IO ()
main = do
    args <- getArgs
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.connect dbPath
    when (args == ["--init"]) $ do
        DB.with db DB.initSchema
    scotty 3000 $ do
        traverse_ (handle db) Route.allRoutes
        notFound $ do
            status status404
            elmPage

handle db rt = do
    let method = case Route.method rt of
            GET  -> get
            POST -> post
    method (Route.path rt) $ handleRt db rt

handleRt :: DB.DB -> Route -> ActionM ()
handleRt _db Root = elmPage
handleRt _db Script = do
    setHeader "Content-Type" "application/javascript"
    file "ui/ui.js"
handleRt _db (NewEvent GET) = elmPage
handleRt db AllEvents = do
    events <- liftIO $ DB.with db DB.allEvents
    json $
        [ ApiTypes.Event
            { ApiTypes.summary = DB.evSummary e
            , ApiTypes.start = DB.evDTStart e
            , ApiTypes.end = DB.evDTStart e -- TODO: actually add the end field to the db.
            , ApiTypes.recurs = []
            }
        | e <- events
        ]
handleRt db (NewEvent POST) = do
    ev <- jsonData
    eid <- liftIO $ DB.with db $ DB.addEvent DB.Event
        { DB.evId = def
        , DB.evSummary = ApiTypes.summary ev
        , DB.evDTStart = ApiTypes.start ev
        }
    json (show eid)
