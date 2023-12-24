{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module AST
  ( Expr (..),
    pattern LambdaExpr,
    pattern AppExpr,
    pattern IfExpr,
    pattern TrueExpr,
    pattern FalseExpr,
    pattern AndExpr,
    pattern OrExpr,
    desugar,
    typeOf,
  )
where

import Data.Int (Int32)
import Data.Text qualified as T

data Typ
  = NilType
  | BoolType
  | CharType
  | FixnumType
  | SymbolType
  | StringType
  | ArrowType Typ Typ
  | Void
  deriving (Show, Eq)

data Expr
  = LambdaExprT !Typ ![Expr] !Expr
  | AppExprT !Typ !Expr ![Expr]
  | StringExpr !T.Text
  | SymbolExpr !T.Text
  | FixnumExpr !Int32
  | CharExpr !Char
  | BoolExpr !Bool
  | NilExpr
  deriving (Show, Eq)

typeOf :: Expr -> Typ
typeOf (LambdaExprT t _ _) = t
typeOf (AppExprT t _ _) = t
typeOf (StringExpr _) = StringType
typeOf (SymbolExpr _) = SymbolType
typeOf (FixnumExpr _) = FixnumType
typeOf (CharExpr _) = CharType
typeOf (BoolExpr _) = BoolType
typeOf NilExpr = NilType

pattern LambdaExpr :: [Expr] -> Expr -> Expr
pattern LambdaExpr args body <- LambdaExprT _ args body
  where
    LambdaExpr args body = LambdaExprT Void args body

pattern AppExpr :: Expr -> [Expr] -> Expr
pattern AppExpr rator rands <- AppExprT _ rator rands
  where
    AppExpr rator rands = AppExprT Void rator rands

pattern IfExpr :: Expr -> Expr -> Expr -> Expr
pattern IfExpr t c a <- AppExprT _ (SymbolExpr "if") [t, c, a]
  where
    IfExpr t c a = AppExprT Void (SymbolExpr "if") [t, c, a]

pattern AndExpr :: [Expr] -> Expr
pattern AndExpr rands = (AppExpr (SymbolExpr "and") rands)

pattern OrExpr :: [Expr] -> Expr
pattern OrExpr rands = (AppExpr (SymbolExpr "or") rands)

pattern TrueExpr :: Expr
pattern TrueExpr = BoolExpr True

pattern FalseExpr :: Expr
pattern FalseExpr = BoolExpr False

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
