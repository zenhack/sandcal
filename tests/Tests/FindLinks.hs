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
    , ( "www.gnu.org."
      , [Link "https://www.gnu.org." "www.gnu.org."]
      )
    , ( "Hey look, www.itsawebsite.com. isn't that cool?"
      , [ Text "Hey look, "
        , Link "https://www.itsawebsite.com." "www.itsawebsite.com."
        , Text " isn't that cool?"
        ]
      )
    , ( "https://gnu.org."
      , [Link "https://gnu.org." "https://gnu.org."]
      )
    , ( "Hey look, www.itsawebsite.com; isn't that cool?"
      , [ Text "Hey look, "
        , Link "https://www.itsawebsite.com" "www.itsawebsite.com"
        , Text "; isn't that cool?"
        ]
      )
    , ( "Reach out at alice@example.com."
      , [ Text "Reach out at "
        , Link "mailto:alice@example.com." "alice@example.com."
        ]
      )
    , ( "Reach out at bob.+.alice@foo.example.net; we'd love to hear from you."
      , [ Text "Reach out at "
        , Link "mailto:bob.+.alice@foo.example.net" "bob.+.alice@foo.example.net"
        , Text "; we'd love to hear from you."
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