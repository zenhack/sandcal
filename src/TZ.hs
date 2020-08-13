module TZ where

import Zhp

import qualified Data.Aeson               as Aeson
import qualified Data.Text.Encoding       as T
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time.Zones.All      (TZLabel, fromTZName, toTZName)

instance Aeson.ToJSON TZLabel where
    toJSON = toTZName
        >>> T.decodeUtf8With lenientDecode
        >>> Aeson.String

instance Aeson.FromJSON TZLabel where
    parseJSON (Aeson.String txt) =
        case fromTZName (T.encodeUtf8 txt) of
            Nothing  -> empty
            Just lbl -> pure lbl
    parseJSON _ = empty
