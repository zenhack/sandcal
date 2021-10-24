-- | Find and mark up links inside of plain text.
module FindLinks
    ( Renderer(..)
    , renderWithLinks
    ) where

import Zhp hiding (many)

import qualified Data.Text.Lazy as LT

import Data.List            (intersperse)
import Data.Void            (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type NodeParser = NodeType -> LT.Text -> Parser Node
type Parser = Parsec Void LT.Text

data Renderer a = Renderer
    { renderText :: LT.Text -> a
    -- ^ Render plain text
    , renderLink :: LT.Text -> LT.Text -> a
    -- ^ Render a link. The first argument is a url, the second is the text
    -- that it was derived from.
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
    deriving(Show, Read, Eq)

data Node = Node
    { nodeType  :: !NodeType
    , nodeValue :: LT.Text
    }
    deriving(Show, Read, Eq)

renderNode :: Renderer a -> Node -> a
renderNode r node =
    let v = nodeValue node in
    case nodeType node of
        Text   -> renderText r v
        HTTP   -> renderLink r v v
        WWW    -> renderLink r ("https://" <> v) v
        Mailto -> renderLink r v v
        Email  -> renderLink r ("mailto:" <> v) v

breakPoint :: NodeType -> LT.Text -> NodeParser -> Parser Node
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

pHex :: Parser LT.Text
pHex = LT.pack . pure <$> satisfy isHexDigit

takeWhilePOrEscaped :: String -> (Char -> Bool) -> Parser LT.Text
takeWhilePOrEscaped name p =
    mconcat <$> many (takeWhile1P (Just name) p <|> pEscaped)

pEscaped :: Parser LT.Text
pEscaped = try $ chain [ string "%", pHex, pHex ]

isSegChar, isSearchChar, isFragmentChar :: Char -> Bool
isSegChar c =
    isUnreserved c
    || c `elem` (";:@^=" :: String)
isSearchChar = isSegChar
isFragmentChar = isSegChar

pHttpUrl :: Parser Node
pHttpUrl = try $ do
    prefix <- chain
        [ string "http"
        , fromMaybe "" <$> optional (string "s")
        , string "://"
        ]
    pHttpUrlTail HTTP prefix

pWWWUrl :: Parser Node
pWWWUrl = try $ do
    prefix <- string "www."
    pHttpUrlTail WWW prefix

pHttpUrlTail :: NodeParser
pHttpUrlTail typ prefix = do
    host <- pHost
    breakPoint typ (prefix <> host) pHostPortTail

pHostPortTail :: NodeParser
pHostPortTail typ prefix = do
    port <- fromMaybe "" <$> optional pPort
    breakPoint typ (prefix <> port) pPostPortTail

pHost :: Parser LT.Text
pHost = do
    prefix <- chain [ pHostPart, string "." ]
    parts <- pHostPart `sepBy1` string "."
    pure $ mconcat $ prefix : intersperse "." parts

pHostPart :: Parser LT.Text
pHostPart = takeWhileP
    (Just "HostPart")
    (\c -> isAlphaNum c || c == '-')

pPort :: Parser LT.Text
pPort = try $ chain
    [ string ":"
    , takeWhileP (Just "digit") isDigit
    ]

pPostPortTail :: NodeParser
pPostPortTail t p = choice
    [ char '/' *> pPath t p
    , char '?' *> pQuery t p
    , char '#' *> pFragment t p
    ]

pPath :: NodeParser
pPath typ prefix = do
    segments <- pPathSegment `sepBy` (char '/')
    let path = mconcat $ intersperse "/" segments
    breakPoint typ (prefix <> "/" <> path) $ \t p -> choice
        [ char '?' *> pQuery t p
        , char '#' *> pFragment t p
        ]

pPathSegment :: Parser LT.Text
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

pNode :: Parser Node
pNode = choice
    [ pHttpUrl
    , pWWWUrl
    -- TODO: mailto
    , pChar
    ]

pChar :: Parser Node
pChar = do
    c <- anySingle
    pure $ Node Text $ LT.pack [c]

toNodes :: LT.Text -> [Node]
toNodes txt =
    case parse (many pNode) "" txt of
        Left e  -> error (show e)
        Right v -> mergeTexts v

mergeTexts :: [Node] -> [Node]
mergeTexts [] = []
mergeTexts [x] = [x]
mergeTexts (x:y:zs) = case (nodeType x, nodeType y) of
    (Text, Text) ->
        mergeTexts $ Node Text (nodeValue x <> nodeValue y) : zs
    _ -> x : mergeTexts (y:zs)
