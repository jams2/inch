{-# LANGUAGE ImportQualifiedPost #-}

module AST (Expr (..)) where

import Data.Int (Int64)
import Data.Text qualified as T

data Expr
  = Lambda ![Expr] !Expr
  | Symbol !T.Text
  | App !Expr ![Expr]
  | Char !Char
  | String !T.Text
  | Fixnum !Int64
  | Bool !Bool
  | Nil
  deriving (Show, Eq)
