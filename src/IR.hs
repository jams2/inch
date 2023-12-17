module IR (IR (..), Constant (..), lower) where

import AST
import Data.Int (Int64)

data IR
  = Noop
  | Constant !Constant
  deriving (Show)

data Constant
  = Nil
  | True
  | False
  | Fixnum !Int64
  deriving (Show)

lower :: Expr -> IR
lower AST.Nil = Constant IR.Nil
lower (AST.Bool Prelude.True)  = Constant IR.True
lower (AST.Bool Prelude.False)  = Constant IR.False
lower (AST.Fixnum i) = Constant (IR.Fixnum i)
lower _ = Noop
