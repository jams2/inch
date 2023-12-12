{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Emit

type Program = [Line]

instance Emit Program where
  toByteString = BS.intercalate "\n" . map toByteString

data Line
  = Instruction !Instruction
  | Label !T.Text
  | TextDirective
  | GlobalDirective !T.Text
  | TypeDirective !T.Text !SymbolType
  deriving (Show)

instance Emit Line where
  toByteString (Instruction i) = toByteString i
  toByteString (Label s) = TE.encodeUtf8 (s <> ":")
  toByteString TextDirective = TE.encodeUtf8 $ indent ".text"
  toByteString (GlobalDirective s) = TE.encodeUtf8 (".global " <> s)
  toByteString (TypeDirective s Function) =
    TE.encodeUtf8 $ indent (T.intercalate " " [".type", s, ",", "@function"])
  toByteString (TypeDirective s Object) =
    TE.encodeUtf8 $ indent (T.intercalate " " [".type", s, ",", "@object"])

data SymbolType = Function | Object deriving (Show)

data Instruction
  = Jmp !Operand
  | Mov !Operand !Operand
  | Call !Operand
  | Ret
  | Nop
  deriving (Show)

instance Emit Instruction where
  toByteString _ = "nop"

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

instance ToOperand Immediate where
  toOperand = Immediate
