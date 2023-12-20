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
import Control.Monad.Reader
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T

newtype CompileError = CompileError T.Text deriving (Show)

type CompilationResult = Either CompileError Program

type Env a = Map.HashMap T.Text a

type CompilationEnv = Reader (Env Primitive) CompilationResult

-- | arity, compiler
data Primitive = Primitive !Int !([Expr] -> CompilationEnv)

compileAll :: Expr -> CompilationResult
compileAll p = (<> [ret]) . (compilePrologue <>) <$> runReader (compile p) primitives

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

compile :: Expr -> CompilationEnv
compile NilExpr = return $ Right [mov NilI RAX]
compile TrueExpr = return $ Right [mov TrueI RAX]
compile FalseExpr = return $ Right [mov FalseI RAX]
compile (FixnumExpr i) = return $ Right [mov (FixI i) RAX]
compile (CharExpr c) = return $ Right [mov (CharI c) RAX]
compile (AppExpr (SymbolExpr rator) rands) = do
  compiler <- asks (Map.lookup rator)
  case compiler of
    Nothing -> return $ Left (CompileError $ "Unknown operator: " <> rator)
    Just p -> compilePrimCall rator p rands
compile _ = return $ Right [nop]

compilePrimCall :: T.Text -> Primitive -> [Expr] -> CompilationEnv
compilePrimCall rator (Primitive arity f) rands
  | length rands /= arity =
      return $ Left (CompileError $ "Wrong number of arguments to " <> rator)
  | otherwise = f rands

compileFxadd1 :: [Expr] -> CompilationEnv
compileFxadd1 [rand] = do
  prologue <- compile rand
  return ((<> [add (FixI 1) RAX]) <$> prologue)
compileFxadd1 _ = return $ Left (CompileError "compileFxadd1: invalid invocation")

compileFxsub1 :: [Expr] -> CompilationEnv
compileFxsub1 [rand] = do
  prologue <- compile rand
  return ((<> [sub (FixI 1) RAX]) <$> prologue)
compileFxsub1 _ = return $ Left (CompileError "compileFxsub1: invalid invocation")

compileFixnumToChar :: [Expr] -> CompilationEnv
compileFixnumToChar [rand] = do
  prologue <- compile rand
  return ((<> p) <$> prologue)
  where
    p = [shl shift RAX, Asm.or tag RAX]
    shift = int $ charShift - fxShift
    tag = int charTag
compileFixnumToChar _ = return $ Left (CompileError "compileFixnumToChar: invalid invocation")

compileCharToFixnum :: [Expr] -> CompilationEnv
compileCharToFixnum [rand] = do
  prologue <- compile rand
  return ((<> [shr shift RAX]) <$> prologue)
  where
    shift = int $ charShift - fxShift
compileCharToFixnum _ = return $ Left (CompileError "compileCharToFixnum: invalid invocation")

primitives :: Env Primitive
primitives =
  Map.fromList
    [ ("fxadd1", Primitive 1 compileFxadd1),
      ("fxsub1", Primitive 1 compileFxsub1),
      ("fixnum->char", Primitive 1 compileFixnumToChar),
      ("char->fixnum", Primitive 1 compileCharToFixnum)
    ]
