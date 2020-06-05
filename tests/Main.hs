module Main (main) where

import Zhp

import Test.Hspec

import qualified Tests.Util.Time

main :: IO ()
main = hspec $ parallel $
    Tests.Util.Time.tests
