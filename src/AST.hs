{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module AST
  ( Expr (..),
    pattern TrueExpr,
    pattern FalseExpr,
    pattern AndExpr,
    desugar,
  )
where

import Data.Int (Int32)
import Data.Text qualified as T

data Expr
  = LambdaExpr ![Expr] !Expr
  | SymbolExpr !T.Text
  | AppExpr !Expr ![Expr]
  | CharExpr !Char
  | StringExpr !T.Text
  | FixnumExpr !Int32
  | IfExpr !Expr !Expr !Expr
  | BoolExpr !Bool
  | NilExpr
  deriving (Show, Eq)

pattern TrueExpr :: Expr
pattern TrueExpr = BoolExpr True

pattern FalseExpr :: Expr
pattern FalseExpr = BoolExpr False

pattern AndExpr :: [Expr] -> Expr
pattern AndExpr rands = (AppExpr (SymbolExpr "and") rands)

pattern OrExpr :: [Expr] -> Expr
pattern OrExpr rands = (AppExpr (SymbolExpr "or") rands)

desugar :: Expr -> Expr
desugar (LambdaExpr args body) = LambdaExpr args $ desugar body
-- Conditional expressions
desugar (IfExpr t c a) = IfExpr (desugar t) (desugar c) (desugar a)
{-
TODO: handle shadowing of `and' and `or' names by userspace bindings
TODO: compiling `and' in terms of nested `if' introduces intermediate jmp instructions that
could be simplified
-}
desugar (AndExpr []) = TrueExpr
desugar (AndExpr [x]) = desugar x
desugar (AndExpr (x : xs)) = IfExpr (desugar x) (desugar $ AndExpr xs) FalseExpr
desugar (OrExpr []) = FalseExpr
desugar (OrExpr [x]) = desugar x
-- TODO: duplication of x here may be expensive - let bind the result of evaluating x
desugar (OrExpr (x : xs)) = IfExpr (desugar x) (desugar x) (desugar $ OrExpr xs)
-- AppExpr must come after other patterns that are defined in terms of AppExpr (e.g. AndExpr)
desugar (AppExpr rator rands) = AppExpr (desugar rator) (map desugar rands)
desugar expr = expr
