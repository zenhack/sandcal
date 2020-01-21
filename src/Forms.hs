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

instance Parsable HtmlDate where
    parseParam txt =
        case P.parse dateParser "" txt of
            Left e  -> Left (LT.pack $ show e)
            Right v -> Right v

data NewEvent = NewEvent
    { neStart   :: HtmlDate
    , neSummary :: LT.Text
    }
    deriving(Show, Read, Eq)

instance IsForm NewEvent where
    parseForm = do
        neStart <- param "startDate"
        neSummary <- param "summary"
        pure NewEvent{..}


dateParser = do
    year <- Prelude.read <$> P.count 4 P.digit
    P.char '-'
    month <- Prelude.read <$> P.count 2 P.digit
    P.char '-'
    day <- Prelude.read <$> P.count 2 P.digit
    pure $ HtmlDate year month day
