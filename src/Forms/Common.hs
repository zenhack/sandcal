module Forms.Common
  ( decodeTZLabel,
    decodeTZLabelOr400,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types.Status (status400)
import Util.TZ (TZLabel, fromTZName)
import Web.Scotty
import Zhp

decodeTZLabel :: LBS.ByteString -> Maybe TZLabel
decodeTZLabel = LBS.toStrict >>> fromTZName

decodeTZLabelOr400 :: LBS.ByteString -> ActionM TZLabel
decodeTZLabelOr400 timezone =
  case decodeTZLabel timezone of
    Just tzLabel -> pure tzLabel
    Nothing -> do
      status status400
      text "Invalid time zone."
      finish
