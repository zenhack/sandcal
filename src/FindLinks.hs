-- | Find and mark up links inside of plain text.
--
-- We recognize the following:
--
-- * http and https urls
-- * http urls where the http(s):// is missing, if the first part of the
--   host is www.
-- * mailto: links
-- * bare email addresses (interpreted as mailto: links).
--
-- The syntax recognized is based on RFC1738, but we are a bit more
-- permissive in a few places.
module FindLinks
  ( Renderer (..),
    renderWithLinks,
  )
where

import Data.List (intersperse)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Zhp hiding (many)

type NodeParser = NodeType -> TB.Builder -> Parser Node

type Parser = Parsec Void LT.Text

data Renderer a = Renderer
  { -- | Render plain text
    renderText :: LT.Text -> a,
    -- | Render a link. The first argument is a url, the second is the text
    -- that it was derived from.
    renderLink :: LT.Text -> LT.Text -> a
  }

-- | Render the supplied text using the 'Renderer', after finding the links within it.
renderWithLinks :: Monoid a => Renderer a -> LT.Text -> a
renderWithLinks r input =
  toNodes input
    & map (renderNode r)
    & mconcat

data NodeType
  = Text
  | HTTP
  | WWW
  | Mailto
  | Email
  deriving (Show, Read, Eq)

data Node = Node
  { nodeType :: !NodeType,
    nodeValue :: TB.Builder
  }
  deriving (Show, Eq)

renderNode :: Renderer a -> Node -> a
renderNode r node =
  let v = nodeValue node
   in case nodeType node of
        Text -> text v
        HTTP -> link v v
        WWW -> link ("https://" <> v) v
        Mailto -> link v v
        Email -> link ("mailto:" <> v) v
  where
    link l txt = renderLink r (TB.toLazyText l) (TB.toLazyText txt)
    text txt = renderText r (TB.toLazyText txt)

breakPoint :: NodeType -> TB.Builder -> NodeParser -> Parser Node
breakPoint typ prefix parser =
  try (parser typ prefix) <|> pure (Node typ prefix)

chain :: Monoid a => [Parser a] -> Parser a
chain = fmap mconcat . sequence

isSafe :: Char -> Bool
isSafe = (`elem` ("$-_.+" :: String))

isExtra :: Char -> Bool
isExtra = (`elem` ("!*'()," :: String))

isUnreserved :: Char -> Bool
isUnreserved c =
  isAlphaNum c
    || isSafe c
    || isExtra c

pHex :: Parser TB.Builder
pHex = TB.singleton <$> satisfy isHexDigit

takeWhilePOrEscaped :: String -> (Char -> Bool) -> Parser TB.Builder
takeWhilePOrEscaped name p =
  mconcat
    <$> many
      ( choice
          [ TB.fromLazyText <$> takeWhile1P (Just name) p,
            pEscaped
          ]
      )

pEscaped :: Parser TB.Builder
pEscaped = try $ chain [TB.singleton <$> char '%', pHex, pHex]

isSegChar, isSearchChar, isFragmentChar :: Char -> Bool
isSegChar c =
  isUnreserved c
    || c `elem` (";:@^&=" :: String)
isSearchChar = isSegChar
isFragmentChar = isSegChar

pHttpUrl :: Parser Node
pHttpUrl = try $ do
  prefix <-
    chain $
      map
        (fmap TB.fromLazyText)
        [ string "http",
          fromMaybe "" <$> optional (string "s"),
          string "://"
        ]
  pHttpUrlTail HTTP prefix

pWWWUrl :: Parser Node
pWWWUrl = try $ do
  prefix <- TB.fromLazyText <$> string "www."
  pHttpUrlTail WWW prefix

pHttpUrlTail :: NodeParser
pHttpUrlTail typ prefix = do
  host <- pHost
  breakPoint typ (prefix <> host) pHostPortTail

pHostPortTail :: NodeParser
pHostPortTail typ prefix = do
  port <- fromMaybe "" <$> optional pPort
  breakPoint typ (prefix <> port) pPostPortTail

pHost :: Parser TB.Builder
pHost = do
  prefix <- chain [pHostPart, TB.singleton <$> char '.']
  parts <- pHostPart `sepBy1` string "."
  pure $ mconcat $ prefix : intersperse "." parts

pHostPart :: Parser TB.Builder
pHostPart =
  TB.fromLazyText
    <$> takeWhileP
      (Just "HostPart")
      (\c -> isAlphaNum c || c == '-')

pPort :: Parser TB.Builder
pPort =
  try $
    chain $
      map
        (fmap TB.fromLazyText)
        [ string ":",
          takeWhileP (Just "digit") isDigit
        ]

pPostPortTail :: NodeParser
pPostPortTail t p =
  choice
    [ char '/' *> pPath t p,
      char '?' *> pQuery t p,
      char '#' *> pFragment t p
    ]

pPath :: NodeParser
pPath typ prefix = do
  segments <- pPathSegment `sepBy` (char '/')
  let path = mconcat $ intersperse "/" segments
  breakPoint typ (prefix <> "/" <> path) $ \t p ->
    choice
      [ char '?' *> pQuery t p,
        char '#' *> pFragment t p
      ]

pPathSegment :: Parser TB.Builder
pPathSegment = takeWhilePOrEscaped "Path segment" isSegChar

pQuery :: NodeParser
pQuery typ prefix = do
  query <- takeWhilePOrEscaped "Query string" isSearchChar
  breakPoint typ (prefix <> "?" <> query) $ \t p ->
    char '#' *> pFragment t p

pFragment :: NodeParser
pFragment typ prefix = do
  fragment <- takeWhilePOrEscaped "URL fragment" isFragmentChar
  pure (Node typ (prefix <> "#" <> fragment))

pMailAddr :: NodeParser
pMailAddr typ prefix = do
  ws <- pWord `sepBy1` "."
  breakPoint Text (prefix <> mconcat (intersperse "." ws)) $ \_ p -> do
    addrTail <-
      chain
        [ TB.singleton <$> char '@',
          pHost
        ]
    pure (Node typ (p <> addrTail))
  where
    pWord =
      TB.fromLazyText
        <$> takeWhile1P
          (Just "Email username")
          (not . (`elem` illegalWordChars))
    illegalWordChars :: String
    illegalWordChars = " ()<>@,;:\\\".[]"

pMailtoUrl :: Parser Node
pMailtoUrl = try $ do
  prefix <- TB.fromLazyText <$> string "mailto:"
  pMailAddr Mailto prefix

pEmailUrl :: Parser Node
pEmailUrl = try $ pMailAddr Email ""

pNode :: Parser Node
pNode =
  choice
    [ pHttpUrl,
      pWWWUrl,
      pMailtoUrl,
      pEmailUrl,
      pChar
    ]

pChar :: Parser Node
pChar = Node Text . TB.singleton <$> anySingle

toNodes :: LT.Text -> [Node]
toNodes txt =
  case parse (many pNode) "" txt of
    Left e -> error (show e)
    Right v -> mergeTexts v

mergeTexts :: [Node] -> [Node]
mergeTexts [] = []
mergeTexts [x] = [x]
mergeTexts (x : y : zs) = case (nodeType x, nodeType y) of
  (Text, Text) ->
    mergeTexts $ Node Text (nodeValue x <> nodeValue y) : zs
  _ -> x : mergeTexts (y : zs)
