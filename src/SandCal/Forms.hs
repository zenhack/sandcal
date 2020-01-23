{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module SandCal.Forms
    ( NewEvent(..)
    ) where

import Zhp

import qualified Data.Text.Lazy         as LT
import qualified Data.Time              as Time
import qualified Data.Time.Clock.System as Time
import           GHC.Generics           (Generic)
import qualified Prelude
import qualified Text.Parsec            as P

import Data.Aeson
import Web.Scotty

data HtmlDate
    = HtmlDate Int Int Int
    deriving(Show, Read, Eq)

data HtmlTime
    = HtmlTime Int Int
    deriving(Show, Read, Eq)

parsecParsable :: P.Parsec LT.Text () a -> LT.Text -> Either LT.Text a
parsecParsable p txt =
    case P.parse p "" txt of
        Left e  -> Left (LT.pack $ show e)
        Right v -> Right v

nDigits :: Int -> P.Parsec LT.Text () Int
nDigits n =
    Prelude.read <$> P.count n P.digit

instance Parsable HtmlDate where
    parseParam = parsecParsable $ do
        year <- nDigits 4
        void $ P.char '-'
        month <- nDigits 2
        void $ P.char '-'
        day <- nDigits 2
        pure $ HtmlDate year month day

instance Parsable HtmlTime where
    parseParam = parsecParsable $ do
        hour <- nDigits 2
        void $ P.char ':'
        minute <- nDigits 2
        pure $ HtmlTime hour minute

data NewEventJson = NewEventJson
    { startDate :: LT.Text
    , startTime :: LT.Text
    , endTime   :: LT.Text
    , summary   :: LT.Text
    }
    deriving(Show, Read, Eq, Generic)
instance FromJSON NewEventJson

data NewEvent = NewEvent
    { neStart   :: Int
    , neEnd     :: Int
    , neSummary :: LT.Text
    }
    deriving(Show, Read, Eq)
instance FromJSON NewEvent where
    parseJSON v = do
        nej <- parseJSON v
        let r = do
                startD <- parseParam (startDate nej)
                startT <- parseParam (startTime nej)
                endT <- parseParam (endTime nej)
                pure
                    ( makeUnixTime startD startT Time.utc
                    , makeUnixTime startD endT Time.utc
                    )
        case r of
            Right (s, e) -> pure NewEvent
                { neStart = s
                , neEnd = e
                , neSummary = summary nej
                }
            Left e  -> Prelude.fail (LT.unpack e)


htmlTimeToTOD :: HtmlTime -> Time.TimeOfDay
htmlTimeToTOD (HtmlTime h m) = Time.TimeOfDay h m 0

htmlDateToDay :: HtmlDate -> Time.Day
htmlDateToDay (HtmlDate y m d) =
    Time.fromGregorian (fromIntegral y) m d

makeUnixTime :: HtmlDate -> HtmlTime -> Time.TimeZone -> Int
makeUnixTime date time zone =
    let day = htmlDateToDay date
        tod = htmlTimeToTOD time
        local = Time.LocalTime day tod
        zoned = Time.ZonedTime local zone
    in
    Time.zonedTimeToUTC zoned
    & Time.utcToSystemTime
    & Time.systemSeconds
    & fromIntegral
