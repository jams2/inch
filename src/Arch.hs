{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Arch (Register (..), toText) where

import Data.Text qualified as T
import Emit

data Register
  = RAX
  | AL
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RBP
  | RSP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Show, Eq)

instance Emit Register where
  toText = ("%" <>) . T.toLower . T.pack . show
