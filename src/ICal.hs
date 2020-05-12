{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module ICal
    ( module Text.ICalendar
    ) where

import Zhp

import           Data.Aeson                  (FromJSON(..), ToJSON(..))
import qualified Data.Aeson                  as Aeson
import           Data.Aeson.TH               (defaultOptions, deriveJSON)
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import Codec.MIME.Type (MIMEType, Multipart)
import Network.URI     (URI, URIAuth)
import Text.ICalendar

import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI

import qualified Data.ByteString.Lazy as LBS

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

do
    let drv = deriveJSON defaultOptions
    mconcat <$> (traverse drv)
        [ ''Date
        , ''DateTime
        , ''OtherParam
        , ''OtherParams
        , ''DTStart
        , ''DTStamp
        , ''UID
        , ''ClassValue
        , ''Class
        , ''Created
        , ''VEvent
        , ''Description
        , ''Geo
        , ''LastModified
        , ''Location
        , ''Organizer
        , ''Priority
        , ''Sequence
        , ''EventStatus
        , ''Summary
        , ''TimeTransparency
        , ''URL
        , ''RecurrenceId
        , ''RRule
        , ''DTEnd
        , ''DurationProp
        , ''Attachment
        , ''Attendee
        , ''Categories
        , ''Comment
        , ''Contact
        , ''ExDate
        , ''RequestStatus
        , ''RelatedTo
        , ''Resources
        , ''RDate
        , ''VAlarm
        , ''OtherProperty
        , ''Language
        , ''Range
        , ''Recur
        , ''Duration
        , ''CUType
        , ''RelationshipType
        , ''Role
        , ''Period
        , ''Trigger
        , ''Repeat
        , ''PartStat
        , ''Weekday
        , ''Sign
        , ''AlarmTriggerRelationship
        , ''Frequency

        , ''MIMEType
        , ''Multipart

        , ''URI
        , ''URIAuth
        ]
