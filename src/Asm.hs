{-# LANGUAGE ImportQualifiedPost #-}

module Asm
  ( Program,
    Line (..),
    Instruction (..),
    textDirective,
    globalDirective,
    typeDirective,
    functionDirective,
    label,
    mov,
    call,
    ret,
    (%),
  )
where

import Arch
import Data.Text qualified as T

type Program = [Line]

data Line = Line
  { lineContent :: !LineContent,
    lineIndent :: !Integer
  }
  deriving (Show)

line :: LineContent -> Line
line c = Line {lineContent = c, lineIndent = 0}

data LineContent
  = Instruction !Instruction
  | Label !T.Text
  | TextDirective
  | GlobalDirective !T.Text
  | TypeDirective !T.Text !SymbolType
  deriving (Show)

label :: T.Text -> Line
label = line . Label

textDirective :: Line
textDirective = line TextDirective

globalDirective :: T.Text -> Line
globalDirective = line . GlobalDirective

typeDirective :: T.Text -> SymbolType -> Line
typeDirective name typ = line $ TypeDirective name typ

functionDirective :: T.Text -> Line
functionDirective = (`typeDirective` Function)

data SymbolType = Function | Object deriving (Show)

class ToLineContent a where
  toLineContent :: a -> LineContent

instance ToLineContent LineContent where
  toLineContent = id

data Instruction
  = Jmp !Operand
  | Mov !Operand !Operand
  | Call !Operand
  | Ret
  deriving (Show)

instance ToLineContent Instruction where
  toLineContent = Instruction

mov :: (ToOperand a, ToOperand b) => a -> b -> Line
mov x y = line $ toLineContent $ Mov (toOperand x) (toOperand y)

call :: T.Text -> Line
call = line . toLineContent . Call . Location

ret :: Line
ret = line $ toLineContent Ret

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
