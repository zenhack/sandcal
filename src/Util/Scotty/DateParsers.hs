-- This module provides Web.Scotty.Parsable instances for dates & times as they
-- are provided by browsers' form elements.
--
-- This package defines its own types for parsing purposes, and supplies
-- functions to convert between these types and the ones from from the
-- time package.
module Util.Scotty.DateParsers
  ( TimeOfDay (..),
    Day (..),
    toStdDay,
    fromStdDay,
    toStdTimeOfDay,
    fromStdTimeOfDay,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Web.Scotty as W
import Zhp hiding (some)
import qualified Prelude

type Parser = Parsec Void LT.Text

data TimeOfDay = TimeOfDay
  { hour :: !Int,
    min :: !Int
  }

toStdTimeOfDay :: TimeOfDay -> Time.TimeOfDay
toStdTimeOfDay (TimeOfDay h m) = Time.TimeOfDay h m 0

fromStdTimeOfDay :: Time.TimeOfDay -> TimeOfDay
fromStdTimeOfDay (Time.TimeOfDay h m _) = TimeOfDay h m

instance Show TimeOfDay where
  show (TimeOfDay h m) =
    printf "%02d:%02d" h m

instance Aeson.ToJSON TimeOfDay where
  toJSON = Aeson.toJSON . show

instance Aeson.FromJSON TimeOfDay where
  parseJSON (Aeson.String s) =
    case parseParamWith timeOfDay (LT.fromStrict s) of
      Right v -> pure v
      Left _ -> empty
  parseJSON _ = empty

newtype Day = Day Time.Day

toStdDay :: Day -> Time.Day
toStdDay (Day d) = d

fromStdDay :: Time.Day -> Day
fromStdDay = Day

instance W.Parsable TimeOfDay where
  parseParam = parseParamWith timeOfDay

instance W.Parsable Day where
  parseParam = fmap Day . parseParamWith day

-- | Use the given parsec parser to implement W.Parsable.parseParam
parseParamWith :: Parser a -> LT.Text -> Either LT.Text a
parseParamWith p input =
  case parse p "" input of
    Right v -> Right v
    Left e -> Left $ LT.pack (show e)

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  h <- int
  _ <- char ':'
  m <- int
  guard (h >= 0 && h < 24)
  guard (m >= 0 && m < 60)
  pure $ TimeOfDay h m

day :: Parser Time.Day
day = do
  y <- int
  _ <- char '-'
  m <- int
  _ <- char '-'
  d <- int
  case Time.fromGregorianValid (fromIntegral y) m d of
    Just v -> pure v
    Nothing -> empty

int :: Parser Int
int = Prelude.read <$> some (oneOf ['0' .. '9'])
