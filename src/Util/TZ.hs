-- | Re-export modules from tz & add aeson orphan instances for TZLabel.
module Util.TZ (module TZ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Zones as TZ
import Data.Time.Zones.All as TZ
import Zhp

instance Aeson.ToJSON TZ.TZLabel where
  toJSON =
    TZ.toTZName
      >>> T.decodeUtf8With lenientDecode
      >>> Aeson.String

instance Aeson.FromJSON TZ.TZLabel where
  parseJSON (Aeson.String txt) =
    case TZ.fromTZName (T.encodeUtf8 txt) of
      Nothing -> empty
      Just lbl -> pure lbl
  parseJSON _ = empty
