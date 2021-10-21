module Main (main) where

import Zhp

import Test.Hspec

import qualified Tests.FindLinks
import qualified Tests.Util.Time

main :: IO ()
main = hspec $ parallel $ do
    Tests.Util.Time.tests
    Tests.FindLinks.tests
