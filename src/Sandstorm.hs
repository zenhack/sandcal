{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sandstorm
  ( getUserId,
    getPermissions,
    maybeGetUserId,
    UserId (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types.Status (status401)
import Web.Scotty
import Zhp

newtype UserId = UserId {userIdToText :: Text}
  deriving (Show, Read, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

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

getPermissions :: ActionM [Text]
getPermissions = do
  h <- header "X-Sandstorm-Permissions"
  pure $ case h of
    Nothing -> []
    Just v -> LT.splitOn "," v
