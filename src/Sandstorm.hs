module Sandstorm
    ( getUserId
    , UserId(..)
    ) where

import Zhp

import Data.Text.Lazy            (Text)
import Network.HTTP.Types.Status (status401)
import Web.Scotty

newtype UserId = UserId { userIdToText :: Text }
    deriving(Show, Read, Eq, Ord)

getUserId :: ActionM UserId
getUserId = do
    headerVal <- header "X-Sandstorm-User-Id"
    case headerVal of
        Just v -> pure $ UserId v
        Nothing -> do
            status status401
            text "Error: No X-Sandstorm-User-Id header"
            finish
