{-# LANGUAGE RecordWildCards #-}
module SandCal.Forms
    ( IsForm(..)
    , HtmlDate(..)
    , NewEvent(..)
    ) where

import Zhp

import qualified Data.Text.Lazy         as LT
import qualified Data.Time              as Time
import qualified Data.Time.Clock.System as Time
import qualified Prelude
import qualified Text.Parsec            as P
import           Web.Scotty

class IsForm a where
    parseForm :: ActionM a

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

data NewEvent = NewEvent
    { neStart   :: Int
    , neEnd     :: Int
    , neSummary :: LT.Text
    }
    deriving(Show, Read, Eq)

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

instance IsForm NewEvent where
    parseForm = do
        neSummary <- param "summary"
        date <- param "startDate"
        startTime <- param "startTime"
        endTime <- param "endTime"
        -- FIXME: Don't assume the time is utc.
        let neStart = makeUnixTime date startTime Time.utc
            neEnd = makeUnixTime date endTime Time.utc
        pure NewEvent{..}
