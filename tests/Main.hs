module Main (main) where

import Test.Hspec
import qualified Tests.FindLinks
import qualified Tests.Util.Time
import Zhp

main :: IO ()
main = hspec $
  parallel $ do
    Tests.Util.Time.tests
    Tests.FindLinks.tests
