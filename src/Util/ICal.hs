{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Util.ICal
  ( module Text.ICalendar,
    veventTZLabel,
    veventDuration,
    freqUnitName,
  )
where

import Codec.MIME.Type (MIMEType, Multipart)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import Network.URI (URI, URIAuth)
import Text.ICalendar
import qualified Util.TZ as TZ
import Zhp

veventTZLabel :: VEvent -> Maybe TZ.TZLabel
veventTZLabel ve = do
  DTStartDateTime {dtStartDateTimeValue} <- veDTStart ve
  case dtStartDateTimeValue of
    FloatingDateTime {} -> Nothing
    UTCDateTime {} -> Just TZ.Etc__UTC
    ZonedDateTime {dateTimeZone} ->
      TZ.fromTZName $ TE.encodeUtf8 $ LT.toStrict dateTimeZone

-- | Extract a duration from the event, in a best-effort fashion.
veventDuration :: VEvent -> Maybe Duration
veventDuration ev =
  case veDTEndDuration ev of
    Nothing -> Nothing
    Just (Right (DurationProp v _)) -> Just v
    Just (Left dtEnd) -> do
      dtStart <- veDTStart ev
      case (dtStart, dtEnd) of
        (DTStartDateTime dts _, DTEndDateTime dte _) -> dte `diffDateTime` dts
        _ -> Nothing

diffDateTime :: DateTime -> DateTime -> Maybe Duration
end `diffDateTime` start =
  case (start, end) of
    (FloatingDateTime s, FloatingDateTime e) -> Just $ e `diffLocalTime` s
    (ZonedDateTime s zoneS, ZonedDateTime e zoneE)
      | zoneS == zoneE -> Just $ e `diffLocalTime` s
    (UTCDateTime s, UTCDateTime e) ->
      Just $
        Time.utcToLocalTime Time.utc e
          `diffLocalTime` Time.utcToLocalTime Time.utc s
    _ -> Nothing

diffLocalTime :: Time.LocalTime -> Time.LocalTime -> Duration
end `diffLocalTime` start =
  if end < start
    then (start `diffLocalTime` end) {durSign = Negative}
    else
      let todDiff =
            Time.timeOfDayToTime (Time.localTimeOfDay end)
              - Time.timeOfDayToTime (Time.localTimeOfDay start)
          pico = Time.diffTimeToPicoseconds todDiff
          totalSec = pico `div` (10 ^ 12)
          totalMin = totalSec `div` 60
          totalHour = totalMin `div` 60
       in DurationDate
            { durSign = Positive,
              durDay = fromIntegral $ Time.localDay end `Time.diffDays` Time.localDay start,
              durHour = fromIntegral $ totalHour,
              durMinute = fromIntegral $ totalMin `mod` 60,
              durSecond = fromIntegral $ totalSec `mod` 60
            }

deriving instance Read Frequency

instance ToJSON LBS.ByteString where
  toJSON lbs = Aeson.String $ decodeUtf8 $ LBS.toStrict $ Base64.encode lbs

instance FromJSON LBS.ByteString where
  parseJSON = Aeson.withText "Bytes" $ \txt ->
    pure $ Base64.decodeLenient $ LBS.fromStrict $ encodeUtf8 txt

instance ToJSON s => ToJSON (CI s) where
  toJSON = toJSON . CI.original
  toEncoding = toEncoding . CI.original

instance (CI.FoldCase s, FromJSON s) => FromJSON (CI s) where
  parseJSON v = CI.mk <$> parseJSON v

freqUnitName :: Frequency -> String
freqUnitName = \case
  Secondly -> "second"
  Minutely -> "minute"
  Hourly -> "hour"
  Daily -> "day"
  Weekly -> "week"
  Monthly -> "month"
  Yearly -> "year"

do
  let drv = deriveJSON defaultOptions
  mconcat
    <$> (traverse drv)
      [ ''Date,
        ''DateTime,
        ''OtherParam,
        ''OtherParams,
        ''DTStart,
        ''DTStamp,
        ''UID,
        ''ClassValue,
        ''Class,
        ''Created,
        ''VEvent,
        ''Description,
        ''Geo,
        ''LastModified,
        ''Location,
        ''Organizer,
        ''Priority,
        ''Sequence,
        ''EventStatus,
        ''Summary,
        ''TimeTransparency,
        ''URL,
        ''RecurrenceId,
        ''RRule,
        ''DTEnd,
        ''DurationProp,
        ''Attachment,
        ''Attendee,
        ''Categories,
        ''Comment,
        ''Contact,
        ''ExDate,
        ''RequestStatus,
        ''RelatedTo,
        ''Resources,
        ''RDate,
        ''VAlarm,
        ''OtherProperty,
        ''Language,
        ''Range,
        ''Recur,
        ''Duration,
        ''CUType,
        ''RelationshipType,
        ''Role,
        ''Period,
        ''Trigger,
        ''Repeat,
        ''PartStat,
        ''Weekday,
        ''Sign,
        ''AlarmTriggerRelationship,
        ''Frequency,
        ''MIMEType,
        ''Multipart,
        ''URI,
        ''URIAuth
      ]
