module IR (IR (..), Constant (..), lower) where

import AST

data IR
  = Noop
  | Constant !Constant
  deriving (Show)

data Constant
  = Nil
  | True
  | False
  deriving (Show)

lower :: Expr -> IR
lower AST.Nil = Constant IR.Nil
lower (AST.Bool Prelude.True)  = Constant IR.True
lower (AST.Bool Prelude.False)  = Constant IR.False
lower _ = Noop
