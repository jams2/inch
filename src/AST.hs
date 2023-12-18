{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

module AST (Expr (..), pattern TrueExpr, pattern FalseExpr) where

import Data.Int (Int64)
import Data.Text qualified as T

data Expr
  = LambdaExpr ![Expr] !Expr
  | SymbolExpr !T.Text
  | AppExpr !Expr ![Expr]
  | CharExpr !Char
  | StringExpr !T.Text
  | FixnumExpr !Int64
  | BoolExpr !Bool
  | NilExpr
  deriving (Show, Eq)

pattern TrueExpr :: Expr
pattern TrueExpr = BoolExpr True

pattern FalseExpr :: Expr
pattern FalseExpr = BoolExpr False
