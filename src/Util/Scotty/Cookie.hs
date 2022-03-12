module Util.Scotty.Cookie
  ( getCookie,
    getCookies,
  )
where

import qualified Data.Text.Lazy as LT
import Data.Void (Void)
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Web.Scotty as Ws
import Zhp hiding (many, some)
import Prelude (lookup)

type Parser = Parsec Void LT.Text

getCookies :: Ws.ActionM [(LT.Text, LT.Text)]
getCookies = do
  hdr <- Ws.header "Cookie"
  case hdr of
    Nothing -> pure []
    Just val ->
      case parse cookies "" val of
        Right v -> pure v
        Left _ -> empty

getCookie :: LT.Text -> Ws.ActionM (Maybe LT.Text)
getCookie key = lookup key <$> getCookies

token :: Parser a -> Parser a
token p = p <* space

cookie :: Parser (LT.Text, LT.Text)
cookie = do
  key <- some (char '_' <|> digitChar <|> letterChar)
  _ <- char '='
  val <- some (noneOf [';'])
  pure (LT.pack key, LT.pack val)

cookies :: Parser [(LT.Text, LT.Text)]
cookies = (cookie `sepBy` token (char ';')) <* eof
