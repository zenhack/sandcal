-- | Find and mark up links inside of plain text.
module FindLinks
    ( Renderer(..)
    , renderWithLinks
    ) where

import Zhp hiding (many)

import qualified Data.Text.Lazy as LT

import Data.Void            (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

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

chain :: Monoid a => [Parser a] -> Parser a
chain ps = mconcat <$> sequence ps

btChoice = choice . map try

chainMany :: Monoid a => Parser a -> Parser a
chainMany = fmap mconcat . many . try

sepByKeep :: Monoid a => Parser a -> Parser a -> Parser a
sepByKeep elt sep = opt (sepBy1Keep (try elt) sep)

sepBy1Keep :: Monoid a => Parser a -> Parser a -> Parser a
sepBy1Keep elt sep = chain
    [ elt
    , chainMany (chain [sep, elt])
    ]

isSafe :: Char -> Bool
isSafe = (`elem` ("$-_.+" :: String))

isExtra :: Char -> Bool
isExtra = (`elem` ("!*'()," :: String))

isUnreserved :: Char -> Bool
isUnreserved c =
    isAlphaNum c
    || isSafe c
    || isExtra c

uchar = stringP isUnreserved <|> chain [ string "%", hex, hex ]

hex = stringP isHexDigit

segChar :: Parser LT.Text
segChar = uchar <|> stringP (`elem` (";:@&=" :: String))

searchChar = segChar
fragmentChar = segChar

manyP = takeWhileP Nothing

httpUrl = chain
    [ string "http"
    , opt (string "s")
    , string "://"
    , httpUrlTail
    ]

httpUrlTail = chain
    [ hostPort
    , opt $ chain
        [ string "/"
        , chainMany segChar `sepByKeep` string "/"
        , opt $ chain
            [ string "?"
            , chainMany searchChar
            ]
        ]
    , opt $ chain
        [ string "#"
        , chainMany fragmentChar
        ]
    ]

opt :: Monoid a => Parser a -> Parser a
opt = fmap (fromMaybe mempty) . optional

pNode :: Parser Node
pNode = try pUrl <|> (Node Text <$> stringP (const True))

pUrl :: Parser Node
pUrl = btChoice
    [ Node HTTP <$> httpUrl
    , Node WWW <$> chain [ try (string "www."), httpUrlTail ]
    , Node Mailto <$> mailtoUrl
    ]

host = btChoice
    [ hostname
    , hostnumber
    ]

hostname = chain
    [ chainMany (chain [ domainlabel, string "."])
    , toplabel
    ]

hostnumber = chain
    [ digits
    , string "."
    , digits
    , string "."
    , digits
    , string "."
    , digits
    ]

digits = manyP isDigit

stringP :: (Char -> Bool) -> Parser LT.Text
stringP p = LT.pack . pure <$> satisfy p

domainlabel :: Parser LT.Text
domainlabel = chain
    [ stringP isAlphaNum
    , domainLabelTail
    ]

domainLabelTail :: Parser LT.Text
domainLabelTail = chainMany $ btChoice
    [ chain
        [ string "-"
        , stringP isAlphaNum
        ]
    , stringP isAlphaNum
    ]


toplabel :: Parser LT.Text
toplabel = chain
    [ stringP isLetter
    , domainLabelTail
    ]

hostPort :: Parser LT.Text
hostPort = chain [ host, opt (chain [ string ":", port ]) ]

port :: Parser LT.Text
port = manyP isDigit

mailtoUrl = chain ["mailto:", email]

email = empty -- TODO

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
