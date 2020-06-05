module Tests.Util.Time
    ( tests
    ) where

import Zhp

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

import qualified Data.Time as Time

import Util.Time (weekBounds)


tests =
    describe "Tests for Util.Time" $
        describe "Tests for weekBounds" $ weekBoundsTests


weekBoundsTests = do
    it "Should always return a 7-day interval" $
        hedgehog prop_weekBounds_size
    it "Should always return an interval including the reference point" $
        hedgehog prop_weekBounds_include
    it "Should always return an interval starting at the beginning of the week." $
        hedgehog prop_weekBounds_startDay

-- type WeekBoundsProp = Time.DayOfWeek -> Time.Day -> Bool

weekBoundsProp prop = do
    startOfWeek <- toEnum <$> forAll (Gen.int $ Range.linear 1 7)
    refPoint <- Time.ModifiedJulianDay . fromIntegral <$>
        forAll (Gen.int $ Range.linear (-1000000) 1000000)
    let (start, end) = weekBounds startOfWeek refPoint
    prop startOfWeek refPoint (start, end)

-- prop_weekBounds_size :: WeekBoundsProp
prop_weekBounds_size = weekBoundsProp $ \_ _ (start, end) ->
    Time.addDays 6 start === end

-- prop_weekBounds_include :: WeekBoundsProp
prop_weekBounds_include = weekBoundsProp $ \_ refPoint (start, end) -> do
    diff start (<=) refPoint
    diff refPoint (<=) end

-- prop_weekBounds_startDay :: WeekBoundsProp
prop_weekBounds_startDay = weekBoundsProp $ \startOfWeek _ (start, end) -> do
    Time.dayOfWeek start === startOfWeek
    Time.dayOfWeek end === pred startOfWeek
