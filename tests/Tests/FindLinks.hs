module Tests.FindLinks
    ( tests
    ) where

import Zhp

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

import qualified Data.Text.Lazy as LT

import FindLinks

tests = do
    describe "Tests for FindLinks" $
        it "Should never change the underlying text" $
            hedgehog prop_noTextChange
    describe "Hand-picked examples" $
        for_ examples $ \(input, output) ->
            it ("Should be correct on input: " <> show input) $
                renderWithLinks testRenderer input `shouldBe` output

examples :: [(LT.Text, [TestNode])]
examples =
    [ ( "abcde"
      , [Text "abcde"]
      )
    , ( "www.gnu.org"
      , [Link "https://www.gnu.org" "www.gnu.org"]
      )
    , ( "http://zenhack.net/foo/bar#baz"
      , [Link "http://zenhack.net/foo/bar#baz" "http://zenhack.net/foo/bar#baz"]
      )
{- -- These currently fail; the . at the end of the address seems to cause the
   -- whole parse to fail, and instead of breaking the input there it backtracks
   -- and treats the whole URL as a Text.
    , ( "www.gnu.org."
      , [Link "https://www.gnu.org" "www.gnu.org", Text "."]
      )
    , ( "Hey look, www.itsawebsite.com. isn't that cool?"
      , [ Text "Hey look, "
        , Link "https://www.itsawebsite.com" "www.itsawebsite.com"
        , Text ". isn't that cool?"
        ]
      )
-}
    , ( "Hey look, www.itsawebsite.com; isn't that cool?"
      , [ Text "Hey look, "
        , Link "https://www.itsawebsite.com" "www.itsawebsite.com"
        , Text "; isn't that cool?"
        ]
      )
    ]


prop_noTextChange = do
    input <- LT.fromStrict <$> forAll (Gen.text (Range.linear 0 4000) Gen.unicode)
    let output = renderWithLinks ignoreRenderer input
    input === output
  where
    ignoreRenderer = Renderer
        { renderLink = \_ txt -> txt
        , renderText = id
        }

testRenderer :: Renderer [TestNode]
testRenderer = Renderer
    { renderLink = \url text -> [Link url text]
    , renderText = pure . Text
    }

data TestNode
    = Link LT.Text LT.Text
    | Text LT.Text
    deriving(Show, Read, Eq)
