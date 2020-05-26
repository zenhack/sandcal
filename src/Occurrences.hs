{-# LANGUAGE DeriveFunctor #-}
module Occurrences
    ( Occurrence(..)
    , eventOccurrences
    , merge
    ) where

import Zhp

import ICal

import Data.Time (UTCTime)

import qualified Data.Set as Set

data Occurrence a = Occurrence
    { ocItem      :: a
    , ocTimeStamp :: !UTCTime
    }
    deriving(Functor)

merge :: [[Occurrence a]] -> [Occurrence a]
merge = foldl' (mergeOn ocTimeStamp) []

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ xs [] = xs
mergeOn _ [] ys = ys
mergeOn f (x:xs) (y:ys)
    | f x < f y = x : mergeOn f xs (y:ys)
    | otherwise = y : mergeOn f (x:xs) ys

eventOccurrences :: VEvent -> [Occurrence VEvent]
eventOccurrences ev =
    let rules = map rRuleValue $ Set.toList (veRRule ev)
        streams = map (ruleOccurrences ev) rules
    in
    Occurrence
        { ocItem = ev
        , ocTimeStamp = dtStampValue (veDTStamp ev)
        }
    : merge streams

unboundedOccurrences :: VEvent -> Recur -> [Occurrence VEvent]
unboundedOccurrences _ev _recur =
    -- TODO
    []

ruleOccurrences :: VEvent -> Recur -> [Occurrence VEvent]
ruleOccurrences vevent recur =
    let unbounded = unboundedOccurrences vevent recur in
    case recurUntilCount recur of
        Nothing                      -> unbounded
        Just (Right count)           -> take count unbounded
        Just (Left dateAndMaybeTime) -> takeUntil dateAndMaybeTime unbounded


takeUntil :: Either Date DateTime -> [Occurrence a] -> [Occurrence a]
takeUntil _ = id -- TODO
