{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module AST
  ( Expr (..),
    Symbol,
    pattern LetrecExpr,
    pattern LambdaExpr,
    pattern AppExpr,
    pattern IfExpr,
    pattern TrueExpr,
    pattern FalseExpr,
    pattern AndExpr,
    pattern OrExpr,
    pattern LetExpr,
    desugar,
  )
where

import Data.Int (Int32)
import Data.Text qualified as T

type Symbol = T.Text

data Expr
  = ListExpr [Expr]
  | StringExpr T.Text
  | SymbolExpr Symbol
  | FixnumExpr Int32
  | CharExpr Char
  | BoolExpr Bool
  | NilExpr
  deriving (Eq, Show)

pattern LetrecExpr :: [Expr] -> Expr -> Expr
pattern LetrecExpr binders body = ListExpr [SymbolExpr "letrec", ListExpr binders, body]

pattern LambdaExpr :: [Expr] -> Expr -> Expr
pattern LambdaExpr args body = ListExpr [SymbolExpr "λ", ListExpr args, body]

pattern LetExpr :: [Expr] -> Expr -> Expr
pattern LetExpr binders body = ListExpr [SymbolExpr "let", ListExpr binders, body]

pattern IfExpr :: Expr -> Expr -> Expr -> Expr
pattern IfExpr t c a = ListExpr [SymbolExpr "if", t, c, a]

pattern AppExpr :: Expr -> [Expr] -> Expr
pattern AppExpr rator rands = ListExpr (rator : rands)

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
desugar (ListExpr xs) = ListExpr $ map desugar xs
desugar expr = expr
