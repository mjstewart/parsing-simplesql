module Backend where

import qualified AST

class Crud where
  create :: AST.CreateStatement
