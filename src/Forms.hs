{-# LANGUAGE RecordWildCards #-}
module Forms
    ( IsForm(..)
    , HtmlDate(..)
    , NewEvent(..)
    ) where

import Zhp

import qualified Data.Text.Lazy as LT
import qualified Prelude
import qualified Text.Parsec    as P
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
        P.char '-'
        month <- nDigits 2
        P.char '-'
        day <- nDigits 2
        pure $ HtmlDate year month day

instance Parsable HtmlTime where
    parseParam = parsecParsable $ do
        hour <- nDigits 2
        P.char ':'
        minute <- nDigits 2
        pure $ HtmlTime hour minute

data NewEvent = NewEvent
    { neStartDate :: HtmlDate
    , neStartTime :: HtmlTime
    , neEndTime   :: HtmlTime
    , neSummary   :: LT.Text
    }
    deriving(Show, Read, Eq)

instance IsForm NewEvent where
    parseForm = do
        neStartDate <- param "startDate"
        neStartTime <- param "startTime"
        neEndTime <- param "endTime"
        neSummary <- param "summary"
        pure NewEvent{..}
