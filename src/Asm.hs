{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm
  ( Program,
    Line (..),
    Instruction (..),
    SymbolType (..),
    Immediate (..),
    mov,
    call,
    ret,
    nop,
    (%),
  )
where

import Arch
import Data.Bits
import Data.Char
import Data.Int
import Data.Text qualified as T
import Emit
import Formatting qualified as F

type Program = [Line]

instance Emit Program where
  toText = T.intercalate "\n" . map Emit.toText

data Line
  = Instruction !Instruction
  | Label !T.Text
  | TextDirective
  | GlobalDirective !T.Text
  | TypeDirective !T.Text !SymbolType
  deriving (Show)

instance Emit Line where
  toText (Instruction i) = Emit.toText i
  toText (Label s) = s <> ":"
  toText TextDirective = indent ".text"
  toText (GlobalDirective s) = T.intercalate " " [".global", s]
  toText (TypeDirective s Function) =
    indent (T.intercalate " " [".type", s, ",", "@function"])
  toText (TypeDirective s Object) =
    indent (T.intercalate " " [".type", s, ",", "@object"])

data SymbolType = Function | Object deriving (Show)

data Instruction
  = Jmp !Operand
  | Mov !Operand !Operand
  | Call !Operand
  | Ret
  | Nop
  deriving (Show)

instance Emit Instruction where
  toText (Jmp x) = indent $ T.intercalate " " ["jmp", Emit.toText x]
  toText (Mov s d) = indent $ T.intercalate " " ["movq", Emit.toText s, ",", Emit.toText d]
  toText (Call x) = indent $ T.intercalate " " ["call", Emit.toText x]
  toText Ret = indent "ret"
  toText Nop = indent "nop"

mov :: (ToOperand a, ToOperand b) => a -> b -> Line
mov x y = Instruction $ Mov (toOperand x) (toOperand y)

call :: T.Text -> Line
call = Instruction . Call . Location

ret :: Line
ret = Instruction Ret

nop :: Line
nop = Instruction Nop

data Operand
  = Location !T.Text
  | Register !Register
  | Offset !Integer !Register
  | Immediate !Immediate
  deriving (Eq, Show)

instance Emit Operand where
  toText (Location s) = s
  toText (Register r) = Emit.toText r
  toText (Offset i r) = T.pack (show i) <> "(" <> Emit.toText r <> ")"
  toText (Immediate x) = Emit.toText x

class ToOperand a where
  toOperand :: a -> Operand

instance ToOperand Register where
  toOperand = Register

instance ToOperand Operand where
  toOperand = id

(%) :: Integer -> Register -> Operand
(%) = Offset

infixl 4 %

data Immediate
  = Nil
  | True
  | False
  | Char !Char
  | Fixnum !Int64
  deriving (Show, Eq)

fxShift :: Int
fxShift = 2

fxTag :: Int64
fxTag = 0

charShift :: Int
charShift = 8

charTag :: Int64
charTag = 0x0F

immediatePrefix :: F.Format T.Text (Int64 -> T.Text)
immediatePrefix = "$" F.% F.prefixHex

hexFormat :: Int64 -> T.Text
hexFormat = F.sformat immediatePrefix

instance Emit Immediate where
  toText Nil = hexFormat 0x3F
  toText Asm.False = hexFormat 0x2F
  toText Asm.True = hexFormat 0x6F
  toText (Fixnum i) = hexFormat (shiftL i fxShift .|. fxTag)
  toText (Char c) = hexFormat (shiftL (val c) charShift .|. charTag)
    where
      val :: Char -> Int64
      val = fromIntegral . ord

instance ToOperand Immediate where
  toOperand = Immediate
