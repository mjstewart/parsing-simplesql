module Parser where


import qualified Control.Exception as Ex
import AST
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L2
import qualified Lexer as L
import Prelude hiding (many, some)

type Parser = Parsec Void Text

tokenP :: Parser SToken
tokenP =
  L.lexeme $
    choice
      [ try keywordP,
        identifierP,
        quotedIdentifierP,
        stringConstantP,
        numericP
      ]

keywordsP :: Parser AST.Keyword
keywordsP =
  choice
    [ AST.KSelect <$ L.keyword "select",
      AST.KFrom <$ L.keyword "from",
      AST.KAs <$ L.keyword "as",
      AST.KTable <$ L.keyword "table",
      AST.KCreate <$ L.keyword "create",
      AST.KInsert <$ L.keyword "insert",
      AST.KInto <$ L.keyword "into",
      AST.KValues <$ L.keyword "values",
      AST.KInt <$ L.keyword "int",
      AST.KText <$ L.keyword "text"
    ]

-- | Tokens such as SELECT, UPDATE, or VALUES are examples of key words
-- Tokens like my_table are identifiers. They identify names of tables, columns, or other database objects,
-- depending on the command they are used in. Therefore they are sometimes simply called "names".
-- Key words and identifiers have the same lexical structure, meaning that one cannot know whether a token is an
-- identifier or a key word without knowing the language. This is why keyword is attempted first.
keywordP :: Parser SToken
keywordP =
  SKeyword <$> keywordsP <?> "Keyword"

identifierP :: Parser SToken
identifierP =
  SIdentifier <$> L.identifier <?> "Identifier"

-- | The quoted identifier is formed by enclosing an arbitrary sequence of characters
--  in double-quotes.
--  A quoted identifier is always an identifier, never a keyword.
--  So "select" could be used to refer to a column or table named "select"
--  whereas an unquoted select would be taken as a keyword.
--  To escape a double quote, use a double quote! "hell""o"
quotedIdentifierP :: Parser SToken
quotedIdentifierP =
  SQuotedIdentifier <$> L.dquotedLiteral <?> "Quoted Identifer"

-- | A string constant in SQL is an arbitrary sequence of characters bounded by
--   single quotes ('), for example 'This is a string'.
--   To include a single-quote character within a string constant,
--   write two adjacent single quotes, e.g., 'Dianne''s horse'.
stringConstantP :: Parser SToken
stringConstantP =
  SStringConstant <$> L.squotedLiteral <?> "Constant"

numericP :: Parser SToken
numericP =
  choice
    [ try $ SNumeric <$> L.double,
      try $ SNumeric <$> L.doubleFromDot,
      SInteger <$> L.integer
    ]

tokenCsvParensP :: Parser [SToken]
tokenCsvParensP =
  L.parens $ tokenP `sepBy` L.comma

columnDefP :: Parser ColumnDef
columnDefP =
  ColumnDef <$> tokenP <*> tokenP

{- The monad style seems to read nicer then the applicative style in this instance.
-}

selectP :: Parser Statement
selectP = do
  L.keyword "select"
  cols <- tokenP `sepBy` L.comma
  L.keyword "from"
  table <- tokenP <* L.semi <* eof
  pure $ SelectStatement cols table

insertP :: Parser Statement
insertP = do
  L.keyword "insert" *> L.keyword "into"
  table <- tokenP
  columns <- optional tokenCsvParensP
  L.keyword "values"
  columnValues <- tokenCsvParensP
  L.semi <* eof
  pure $ InsertStatement table columns columnValues

createTableP :: Parser Statement
createTableP = do
  L.keyword "create" *> L.keyword "table"
  tableName <- tokenP
  columnDefs <- L.parens $ columnDefP `sepBy` L.comma
  L.semi <* eof
  pure $ CreateStatement tableName columnDefs


-- | Applicative style, it gets abit noisy..
selectP' :: Parser Statement
selectP' =
  L.keyword "select"
    *> ( SelectStatement
           <$> (tokenP `sepBy` L.comma)
           <*> (L.keyword "from" *> tokenP)
           <* L.semi
           <* eof
       )

-- | INSERT INTO purchase_orders (cola, colb) values (a, b, c);
insertP' :: Parser Statement
insertP' =
  L.keyword "insert"
    *> L.keyword "into"
    *> ( InsertStatement
           <$> tokenP
           <*> (optional tokenCsvParensP)
           <*> ( (L.keyword "values")
                   *> tokenCsvParensP
                   <* L.semi
                   <* eof
               )
       )

createTableP' :: Parser Statement
createTableP' =
  L.keyword "create"
    *> L.keyword "table"
    *> ( CreateStatement
           <$> tokenP
           <*> (L.parens $ columnDefP `sepBy` L.comma)
       )
    <* L.semi
    <* eof

parseFile :: FilePath -> IO ()
parseFile path = do
  res <- Ex.try $ readFileText path :: IO (Either SomeException Text)
  case res of
    Right source ->
      case (runParser createTableP path source) of
        Right t -> putStrLn $ show t
        Left bundle -> putStrLn $ errorBundlePretty bundle
    Left e -> putStrLn $ "Error with file: " <> path
