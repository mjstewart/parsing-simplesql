module Lexer where

import qualified AST
import qualified Data.Char
import qualified Data.Text as Text
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

-- https://notes.eatonphil.com/database-basics.html
-- https://www.postgresql.org/docs/current/sql-syntax-lexical.html

type Parser = Parsec Void Text

-- TODO: Symbol for text type keyword?
sc :: Parser ()
sc =
  L.space Char.space1 line block
  where
    line = L.skipLineComment "--"
    block = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme sc

symbol :: Text -> Parser Text
symbol =
  L.symbol' sc

parens :: Parser a -> Parser a
parens =
  between (symbol "(") (symbol ")")

squote :: Parser Char
squote =
  single '\'' <?> "single quote"

dquote :: Parser Char
dquote =
  single '"' <?> "double quote"

dquotes :: Parser a -> Parser a
dquotes =
  between dquote dquote

squotes :: Parser a -> Parser a
squotes =
  between squote squote

semi :: Parser ()
semi =
  void $ symbol ";"

star :: Parser ()
star =
  void $ symbol "*"

dot :: Parser ()
dot =
  void $ symbol "."

comma :: Parser ()
comma =
  void $ symbol ","

underscore :: Parser Char
underscore =
  single '_' <?> "underscore"

squotedLiteral :: Parser Text
squotedLiteral =
  lexeme $ squotes p <&> Text.pack
  where
    p = many $ ('\'' <$ Char.string "''") <|> anySingleBut '\''

dquotedLiteral :: Parser Text
dquotedLiteral =
  lexeme $ dquotes p <&> Text.pack
  where
    p = many $ ('"' <$ Char.string "\"\"") <|> anySingleBut '\"'

keyword :: Text -> Parser ()
keyword word =
  void $ lexeme $ Char.string' word <* notFollowedBy Char.alphaNumChar

identifier :: Parser Text
identifier =
  lexeme $
    Text.cons
      <$> firstChar
      <*> takeWhileP (Just "Alpha numeric char or underscore") rest
  where
    firstChar = Char.letterChar <|> underscore
    rest x = Data.Char.isAlphaNum x || x == '_'

integer :: Parser Int
integer =
  L.signed sc L.decimal

double :: Parser Double
double =
  L.signed sc L.float

-- .4 => 0.4
-- 4. => 4.0
doubleFromDot :: Parser Double
doubleFromDot =
  L.signed sc $ convert $ leading <|> trailing
  where
    leading, trailing :: Parser Text
    leading = (dot *> L.decimal :: Parser Int) >>= pure . ("0." <>) . show
    trailing = (L.decimal <* dot :: Parser Int) >>= pure . (<> ".0") . show
    convert :: Parser Text -> Parser Double
    convert p = do
      intText <- p
      maybe
        (fail $ "failed to convert to Double: " <> show intText)
        pure
        $ readMaybe (Text.unpack intText)

withBoundary :: Parser a -> Parser a
withBoundary p =
  sc *> p <* eof
