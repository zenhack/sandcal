module Main (main) where

import Zhp

import qualified Data.Text.Lazy as LT

import Control.Exception.Safe        (bracket)
import Database.Selda.SQLite         (withSQLite)
import Text.Blaze.Html               (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty

import qualified DBModel

blaze :: Html -> ActionM ()
blaze = html . renderHtml

data Route
    = Root
    | NewEvent GetPost

data GetPost = GET | POST



formatRoute :: Route -> LT.Text
formatRoute Root         = "/"
formatRoute (NewEvent _) = "/event/new"

main :: IO ()
main = do
    dbPath <- maybe "./sandcal.db" id <$> lookupEnv "DB_PATH"
    withSQLite dbPath $ DBModel.initDB
    scotty 3000 $ do
        get "/" $ handleRt Root
        get "/event/new" $ handleRt (NewEvent GET)

handleRt :: Route -> ActionM ()
handleRt Root = do
    text "Hello, World!"
handleRt (NewEvent GET) =
    text "TODO"
handleRt (NewEvent POST) =
    text "TODO"
