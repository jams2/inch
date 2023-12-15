{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile
  ( CompilationResult,
    compileFunctionHeader,
    compilePrologue,
    compile,
    compileAll,
  )
where

import AST
import Arch
import Asm
import Data.Text qualified as T
import IR

newtype CompileError = CompileError T.Text deriving (Show)

type CompilationResult = Either CompileError Program

compileAll :: Expr -> CompilationResult
compileAll p = (<> [ret]) . (compilePrologue <>) <$> compile (lower p)

compileFunctionHeader :: T.Text -> Program
compileFunctionHeader name =
  [ TextDirective, -- .text
    GlobalDirective name, -- .global <name>
    TypeDirective name Function, -- .type <name>, @function
    Label name -- <name>:
  ]

-- | Initialize register state
--  Args passed to scheme_entry by the C runtime:
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

compile :: IR -> CompilationResult
compile Noop = Right [nop]
compile (Constant IR.Nil) = Right [mov Asm.Nil RAX]
compile (Constant IR.True) = Right [mov Asm.True RAX]
compile (Constant IR.False) = Right [mov Asm.False RAX]
