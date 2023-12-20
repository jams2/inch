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
    call,
    ret,
    nop,
    (%),
    charShift,
    charTag,
    fxShift,
  )
where

import Arch
import Data.Bits
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
  | Add !Operand !Operand
  | Sub !Operand !Operand
  | Shl !Operand !Operand
  | Shr !Operand !Operand
  | Or !Operand !Operand
  | Jmp !Operand
  | Call !Operand
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
  toText (Or s d) = formatBinary "orq" s d
  toText (Jmp d) = formatUnary "jmp" d
  toText (Call d) = formatUnary "call" d
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
  | FixI !Int64
  | IntI !Int
  deriving (Show, Eq)

int :: Integral a => a -> Immediate
int i = IntI $ fromIntegral i

fxShift :: Int
fxShift = 2

fxTag :: Int64
fxTag = 0

charShift :: Int
charShift = 8

charTag :: Int64
charTag = 0x0F

immediatePrefix :: (Integral a) => F.Format T.Text (a -> T.Text)
immediatePrefix = "$" F.% F.prefixHex

hexFormat :: (Integral a) => a -> T.Text
hexFormat = F.sformat immediatePrefix

instance Emit Immediate where
  toText (IntI i) = hexFormat i
  toText NilI = hexFormat (0x3F :: Int64)
  toText FalseI = hexFormat (0x2F :: Int64)
  toText TrueI = hexFormat (0x6F :: Int64)
  -- Negative Int64s cause an error when hex formatting, cast to a Word64
  toText (FixI i) = hexFormat $ toWord (shiftL i fxShift .|. fxTag)
    where
      toWord :: Int64 -> Word64
      toWord = fromIntegral
  toText (CharI c) = hexFormat (shiftL (val c) charShift .|. charTag)
    where
      val :: Char -> Int64
      val = fromIntegral . ord

instance ToOperand Immediate where
  toOperand = Immediate
