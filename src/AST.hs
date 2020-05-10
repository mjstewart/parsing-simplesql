module AST where

data SToken
  = SKeyword Keyword
  | SIdentifier Text
  | SQuotedIdentifier Text
  | SStringConstant Text
  | SInteger Int
  | SNumeric Double
  deriving (Show, Eq)

data Keyword
  = KSelect
  | KFrom
  | KAs
  | KTable
  | KCreate
  | KInsert
  | KInto
  | KValues
  | KInt
  | KText
  deriving (Show, Eq, Read)

data Program
  = Program [Statement]

data ColumnDef
  = ColumnDef
      { columnName :: SToken,
        columnType :: SToken
      }
  deriving (Show, Eq)

data Statement
  = SelectStatement
      { _selectColumns :: [SToken],
        _selectFrom :: SToken
      }
  | InsertStatement
      { _insertTable :: SToken,
        _insertColumns :: Maybe [SToken],
        _insertValues :: [SToken]
      }
  | CreateStatement
      { _createTable :: SToken,
        _createColumnDef :: [ColumnDef]
      }
  deriving (Show, Eq)
