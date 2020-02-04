{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main (main) where

import Zhp

import Database.Selda (def, fromId)

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

do404 = do
    status status404
    elmPage

main :: IO ()
main = do
    args <- getArgs
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.connect dbPath
    when (args == ["--init"]) $ do
        DB.with db DB.initSchema
    scotty 3000 $ do
        traverse_ (handle db) Route.allRoutes
        get "/event/:eid" $ do
            eid <- param "eid"
            handleRt db (Route.Event eid)
        get "/api/event/:eid" $ do
            eid <- param "eid"
            res <- liftIO $ DB.with db (DB.getEvent eid)
            case res of
                Nothing -> do404
                Just (e, rs) ->
                    json $ ApiTypes.Event
                        { ApiTypes.summary = DB.evSummary e
                        , ApiTypes.start = DB.evDTStart e
                        , ApiTypes.end = DB.evDTStart e
                        , ApiTypes.id = Just (fromId $ DB.evId e)
                        , ApiTypes.recurs =
                            [ ApiTypes.Recur
                                { ApiTypes.until = DB.rUntil r
                                , ApiTypes.frequency = DB.rFrequency r
                                }
                            | r <- rs ]
                        }
        notFound $ do404

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
handleRt _db (Event _) = elmPage
handleRt db AllEvents = do
    events <- liftIO $ DB.with db DB.allEvents
    json $
        [ ApiTypes.Event
            { ApiTypes.summary = DB.evSummary e
            , ApiTypes.start = DB.evDTStart e
            , ApiTypes.end = DB.evDTStart e -- TODO: actually add the end field to the db.
            , ApiTypes.recurs = []
            , ApiTypes.id = Just (fromId $ DB.evId e)
            }
        | e <- events
        ]
handleRt db (NewEvent POST) = do
    ev <- jsonData
    eid <- liftIO $ DB.with db $ do
        eid <- DB.addEvent DB.Event
            { DB.evId = def
            , DB.evSummary = ApiTypes.summary ev
            , DB.evDTStart = ApiTypes.start ev
            }
        for_ (ApiTypes.recurs ev) $ \r ->
            DB.addRecur DB.Recur
                { DB.rEventId = eid
                , DB.rFrequency = ApiTypes.frequency r
                , DB.rUntil = ApiTypes.until r
                }
        pure eid
    json (show eid)
