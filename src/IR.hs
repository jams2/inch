module IR (IR (..), Constant (..), lower) where

import AST

data IR
  = Noop
  | Constant !Constant
  deriving (Show)

data Constant
  = Nil
  deriving (Show)

lower :: Expr -> IR
lower AST.Nil = Constant IR.Nil
lower _ = Noop
