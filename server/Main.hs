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

main :: IO ()
main = do
    args <- getArgs
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.connect dbPath
    when (args == ["--init"]) $ do
        DB.with db DB.initSchema
    scotty 3000 $ do
        get "/" elmPage
        get "/ui.js" $ do
            setHeader "Content-Type" "application/javascript"
            file "ui.js"
        get "/event/:eid" elmPage
        get "/event/new" elmPage

        get "/api/all-events.json" $ getAllEvents db
        post "/api/event/new" $ postNewEvent db
        get "/api/event/:eid" $ do
            eid <- param "eid"
            getEvent db eid
        notFound $ do404

elmPage = do
    setHeader "Content-Type" "text/html"
    file "index.html"

getAllEvents db = do
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

getEvent db eid = do
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

postNewEvent db = do
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

do404 = do
    status status404
    elmPage