module ErrorCatalogue where

data Error
  = ErrTableDoesNotExist
  | ErrColumnDoesNotExist
  | ErrInvalidSelectItem
  | ErrInvalidDataType
  | ErrMissingValues
  deriving (Show, Eq)
  
