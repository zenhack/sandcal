module DateParsers
    ( timeOfDay
    , day
    ) where

import qualified Prelude

import Zhp hiding (some)

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text as T
import qualified Data.Time as Time

type Parser = Parsec Void T.Text

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
