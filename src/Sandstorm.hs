{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sandstorm
    ( getUserId
    , maybeGetUserId
    , UserId(..)
    ) where

import Zhp

import qualified Data.Aeson as Aeson

import Data.Text.Lazy            (Text)
import Network.HTTP.Types.Status (status401)
import Web.Scotty

newtype UserId = UserId { userIdToText :: Text }
    deriving(Show, Read, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

maybeGetUserId :: ActionM (Maybe UserId)
maybeGetUserId =
    fmap UserId <$> header "X-Sandstorm-User-Id"

getUserId :: ActionM UserId
getUserId = do
    maybeUserId <- maybeGetUserId
    case maybeUserId of
        Just uid -> pure uid
        Nothing -> do
            status status401
            text "Error: No X-Sandstorm-User-Id header"
            finish
