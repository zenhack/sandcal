{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module ICal.Util
    ( veventTZLabel
    ) where

import Zhp

import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy     as LT
import qualified ICal
import qualified Util.TZ            as TZ

veventTZLabel :: ICal.VEvent -> Maybe TZ.TZLabel
veventTZLabel ve = do
    ICal.DTStartDateTime{dtStartDateTimeValue} <- ICal.veDTStart ve
    case dtStartDateTimeValue of
        ICal.FloatingDateTime{} -> Nothing
        ICal.UTCDateTime{}      -> Just TZ.Etc__UTC
        ICal.ZonedDateTime{dateTimeZone} ->
            TZ.fromTZName $ TE.encodeUtf8 $ LT.toStrict dateTimeZone
