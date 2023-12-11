{-# LANGUAGE ImportQualifiedPost #-}

module Asm
  ( Program,
    Line (..),
    Instruction (..),
    SymbolType (..),
    mov,
    call,
    ret,
    (%),
  )
where

import Arch
import Data.Text qualified as T

type Program = [Line]

data Line
  = Instruction !Instruction
  | Label !T.Text
  | TextDirective
  | GlobalDirective !T.Text
  | TypeDirective !T.Text !SymbolType
  deriving (Show)

data SymbolType = Function | Object deriving (Show)

data Instruction
  = Jmp !Operand
  | Mov !Operand !Operand
  | Call !Operand
  | Ret
  deriving (Show)

mov :: (ToOperand a, ToOperand b) => a -> b -> Line
mov x y = Instruction $ Mov (toOperand x) (toOperand y)

call :: T.Text -> Line
call = Instruction . Call . Location

ret :: Line
ret = Instruction Ret

data Operand
  = Location !T.Text
  | Register !Register
  | Offset !Integer !Register
  deriving (Eq, Show)

class ToOperand a where
  toOperand :: a -> Operand

instance ToOperand Register where
  toOperand = Register

instance ToOperand Operand where
  toOperand = id

(%) :: Integer -> Register -> Operand
(%) = Offset

infixl 4 %
