{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm
  ( Program,
    Line (..),
    Instruction (..),
    SymbolType (..),
    Immediate (..),
    int,
    mov,
    add,
    sub,
    shl,
    shr,
    Asm.or,
    Asm.and,
    cmp,
    movzb,
    call,
    ret,
    sete,
    nop,
    (%),
  )
where

import Arch
import Constants qualified as C
import Data.Bits (shiftL, (.|.))
import Data.Char
import Data.Int
import Data.Text qualified as T
import Data.Word (Word64)
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
  = Mov !Operand !Operand
  | Movzb !Operand !Operand
  | Add !Operand !Operand
  | Sub !Operand !Operand
  | Shl !Operand !Operand
  | Shr !Operand !Operand
  | Or !Operand !Operand
  | And !Operand !Operand
  | Cmp !Operand !Operand
  | Jmp !Operand
  | Call !Operand
  | Sete !Operand
  | Ret
  | Nop
  deriving (Show)

formatBinary :: (Emit a, Emit b) => T.Text -> a -> b -> T.Text
formatBinary op s d = indent $ T.intercalate " " [op, Emit.toText s, ",", Emit.toText d]

formatUnary :: (Emit a) => T.Text -> a -> T.Text
formatUnary op d = indent $ T.intercalate " " [op, Emit.toText d]

instance Emit Instruction where
  toText (Mov s d) = formatBinary "movq" s d
  toText (Add s d) = formatBinary "addq" s d
  toText (Sub s d) = formatBinary "subq" s d
  toText (Shl s d) = formatBinary "shlq" s d
  toText (Shr s d) = formatBinary "shrq" s d
  toText (Or s d) = formatBinary "or" s d
  toText (And s d) = formatBinary "andq" s d
  toText (Cmp s d) = formatBinary "cmp" s d
  toText (Movzb s d) = formatBinary "movzbq" s d
  toText (Jmp d) = formatUnary "jmp" d
  toText (Call d) = formatUnary "call" d
  toText (Sete d) = formatUnary "sete" d
  toText Ret = indent "ret"
  toText Nop = indent "nop"

binaryOp ::
  (ToOperand a, ToOperand b) =>
  (Operand -> Operand -> Instruction) -> -- Operand constructor
  a ->
  b ->
  Line
binaryOp x p q = Instruction $ x (toOperand p) (toOperand q)

mov :: (ToOperand a, ToOperand b) => a -> b -> Line
mov = binaryOp Mov

add :: (ToOperand a, ToOperand b) => a -> b -> Line
add = binaryOp Add

sub :: (ToOperand a, ToOperand b) => a -> b -> Line
sub = binaryOp Sub

shl :: (ToOperand a, ToOperand b) => a -> b -> Line
shl = binaryOp Shl

shr :: (ToOperand a, ToOperand b) => a -> b -> Line
shr = binaryOp Shr

or :: (ToOperand a, ToOperand b) => a -> b -> Line
or = binaryOp Or

and :: (ToOperand a, ToOperand b) => a -> b -> Line
and = binaryOp And

cmp :: (ToOperand a, ToOperand b) => a -> b -> Line
cmp = binaryOp Cmp

movzb :: (ToOperand a, ToOperand b) => a -> b -> Line
movzb = binaryOp Movzb

sete :: (ToOperand a) => a -> Line
sete = Instruction . Sete . toOperand

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
  = NilI
  | TrueI
  | FalseI
  | CharI !Char
  | FixI !Int32
  | IntI !Int
  deriving (Show, Eq)

int :: (Integral a) => a -> Immediate
int i = IntI $ fromIntegral i

immediatePrefix :: (Integral a) => F.Format T.Text (a -> T.Text)
immediatePrefix = "$" F.% F.prefixHex

hexFormat :: (Integral a) => a -> T.Text
hexFormat = F.sformat immediatePrefix

instance Emit Immediate where
  toText (IntI i) = hexFormat i
  toText NilI = hexFormat C.nil
  toText FalseI = hexFormat C.false
  toText TrueI = hexFormat C.true
  -- Negative Int32s cause an error when hex formatting, cast to a Word64
  toText (FixI i) = hexFormat $ toWord (shiftL i C.fxShift .|. C.fxTag)
    where
      toWord :: Int32 -> Word64
      toWord = fromIntegral
  toText (CharI c) = hexFormat (shiftL (val c) C.charShift .|. C.charTag)
    where
      val :: Char -> Int32
      val = fromIntegral . ord

instance ToOperand Immediate where
  toOperand = Immediate
