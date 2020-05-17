-- This module provides Web.Scotty.Parsable instances for dates & times as they
-- are provided by browsers' form elements.
--
-- The instances are defined on newtype wrappers around corresponding types
-- from the time package.
module DateParsers
    ( TimeOfDay(..)
    , Day(..)
    ) where

import qualified Prelude

import Zhp hiding (some)

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text.Lazy as LT
import qualified Data.Time      as Time

import qualified Web.Scotty as W

type Parser = Parsec Void LT.Text

newtype TimeOfDay = TimeOfDay Time.TimeOfDay

newtype Day = Day Time.Day

instance W.Parsable TimeOfDay where
    parseParam = fmap TimeOfDay . parseParamWith timeOfDay

instance W.Parsable Day where
    parseParam = fmap Day . parseParamWith day

parseParamWith :: Parser a -> LT.Text -> Either LT.Text a
parseParamWith p input =
    case parse p "" input of
        Right v -> Right v
        Left e  -> Left $ LT.pack (show e)

timeOfDay :: Parser Time.TimeOfDay
timeOfDay = do
    h <- int
    _ <- char ':'
    m <- int
    guard (h >= 0 && h < 24)
    guard (m >= 0 && m < 60)
    pure Time.TimeOfDay
        { Time.todHour = h
        , Time.todMin = m
        , Time.todSec = 0
        }

day :: Parser Time.Day
day = do
    y <- int
    _ <- char '-'
    m <- int
    _ <- char '-'
    d <- int
    case Time.fromGregorianValid (fromIntegral y) m d of
        Just v  -> pure v
        Nothing -> empty

int :: Parser Int
int = Prelude.read <$> some (oneOf ['0'..'9'])
