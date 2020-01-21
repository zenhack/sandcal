{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Zhp

import qualified Data.Text.Lazy as LT

import Control.Exception.Safe        (bracket)
import Database.Selda.SQLite         (withSQLite)
import Text.Blaze.Html               (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty

import Config (cfgDBPath, getConfig)

import qualified DB
import qualified Forms
import           Route (Method(..), Route(..))
import qualified Route
import qualified View

blaze :: Html -> ActionM ()
blaze = html . renderHtml

main :: IO ()
main = do
    args <- getArgs
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.connect dbPath
    when (args == ["--init"]) $ do
        DB.with db DB.initSchema
    scotty 3000 $ traverse_ (handle db)
        [ Root
        , NewEvent GET
        , NewEvent POST
        ]

handle db rt = do
    let method = case Route.method rt of
            GET  -> get
            POST -> post
    method (Route.path rt) $ handleRt db rt

handleRt :: DB.DB -> Route -> ActionM ()
handleRt db Root = do
    events <- liftIO $ DB.with db DB.allEvents
    blaze $ View.page "Sandcal" (pure ()) $ View.events events
handleRt db (NewEvent GET) =
    blaze $ View.page "Sandcal - New Event" (pure ()) View.newEventForm
handleRt db (NewEvent POST) = do
    f <- Forms.parseForm
    liftIO $ print (f :: Forms.NewEvent)
    text "TODO"
