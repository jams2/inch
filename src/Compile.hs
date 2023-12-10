{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile () where

import Arch
import Asm
import Data.Text qualified as T

compileFunctionHeader :: T.Text -> Program
compileFunctionHeader name =
  [ textDirective,
    globalDirective name,
    functionDirective name,
    label name
  ]

-- | Initialize register state
--  RDI <- Context
--  RSI <- stack base
--  RDX <- heap address
compilePrologue :: Program
compilePrologue =
  compileFunctionHeader "scheme_entry"
    <> [ mov RDI RCX, -- store the Context location
         mov RBX (8 % RCX), -- preserve C register state
         mov RBP (48 % RCX),
         mov RSP (56 % RCX),
         mov R12 (96 % RCX),
         mov R13 (104 % RCX),
         mov R14 (112 % RCX),
         mov R15 (120 % RCX),
         mov RSI RSP, -- load stack base
         mov RDX RBP, -- load alloc pointer
         call "L_scheme_entry",
         mov (8 % RCX) RBX, -- restore C register state
         mov (48 % RCX) RBP,
         mov (56 % RCX) RSP,
         mov (96 % RCX) R12,
         mov (104 % RCX) R13,
         mov (112 % RCX) R14,
         mov (120 % RCX) R15,
         ret
       ]
    <> compileFunctionHeader "L_scheme_entry"
