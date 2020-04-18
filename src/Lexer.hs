module Lexer where

import Control.Monad.Combinators ((<|>))
import Data.Bool (bool)
import qualified Data.Char
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as Byte
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | https://www.postgresql.org/docs/current/sql-syntax-lexical.html
data Keyword
  = Select
  | From
  | As
  | Table
  | Create
  | Insert
  | Into
  | Values
  | Int
  | Text
  deriving (Show, Eq)

data SToken
  = SKeyword Keyword
  | SIdentifier Text
  | SQuotedIdentifier Text
  | SConstant Text
  | SNumeric Scientific
  | SInteger Int
  deriving (Show, Eq)

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

semi :: Parser Text
semi =
  symbol ";"

star :: Parser Text
star =
  symbol "*"

dot :: Parser Text
dot =
  symbol "."

comma :: Parser Text
comma =
  symbol ","

underscore :: Parser Char
underscore =
  single '_' <?> "underscore"

-- | A string constant in SQL is an
--    arbitrary sequence of characters bounded by single quotes (').
squotes :: Parser a
squotes =
  quotes '\''

-- | There is a second kind of identifier: the delimited identifier or quoted identifier.
--    Since its in double quotes, its always a known identifier by definition unlike
--    a standard identifier which can be either a keyword or identifier depending on the
--    language used.
--
--    eg "select" is a quoted identifier that could refer to a table or column name,
--    not the keyword SELECT.
--
--    Most commonly used for column names with spaces such as "customer order`"
dquotes :: Parser a
dquotes =
  quotes '\"'

quotes :: Char -> Parser a
quotes quote =
  Char.char quote *> (Text.pack <$> (manyTill L.charLiteral $ Char.char quote))


just simplify

char lit
string lit
string quotes etc. 

-- | Tokens such as SELECT, UPDATE, or VALUES are examples of
--   keywords, that is, words that have a fixed meaning in the SQL language.
--   The tokens MY_TABLE and A are examples of identifiers.
--   They identify names of tables, columns, or other database objects,
--   depending on the command they are used in.
--
--   Keywords and identifiers have the same lexical structure meaning that one cannot know
--   whether a token is an identifier or keyword without knowing the language!
identifier :: Parser Text
identifier =
  Text.cons
    <$> headChar
    <*> takeWhileP (Just "Alpha numeric char or underscore") rest
  where
    headChar = Char.letterChar <|> underscore
    rest x = or $ map (flip ($) x) [Data.Char.isAlphaNum, (== '_')]

-- megaparsec ignores trailing whitespace, this will run a parser that checks for
-- beginning whitespace.
runP :: Parser Text -> Parser Text
runP =
  (Char.space *>)


integer = lexeme L.decimal

-- numberP :: Parser SToken
-- numberP =
  -- choice [
    -- TNumeric <$> (lexeme L.scientific)
  -- , TInteger <$> (lexeme L.decimal)
  -- , TInteger <$> signedInteger
  -- ]
  -- need to handle 1. == 1.0
  -- .1  == 0.1

signedInteger = L.signed sc integer

x :: Parser Double
x =
  toRealFloat <$> L.scientific

y :: Parser Scientific
y =
  L.scientific

orr :: (Foldable t) => t (a -> Bool) -> a -> Bool
orr fs x =
  foldr (\f p -> bool (f x) p p) False fs
{-

b :: Parser Text
b =
   between doubleQuote doubleQuote p
  where
    p = takeWhileP (Just "char literal") g
    g x = isAlphaNum x || isSpace x

-}
