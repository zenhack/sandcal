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
    args <- getArgs
    dbPath <- cfgDBPath <$> getConfig
    db <- DB.connect dbPath
    when (args == ["--init"]) $ do
        DB.withDB db DB.initSchema
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
