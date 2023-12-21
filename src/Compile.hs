{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.State.Lazy
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T

newtype CompileError = CompileError T.Text deriving (Show)

type CompilationResult = Either CompileError Program

type Env a = Map.HashMap T.Text a

newtype CompilationState = CompilationState {primitiveEnv :: Env Primitive}

type CompilationEnv = State CompilationState CompilationResult

-- | arity, compiler
data Primitive = Primitive !Int !([Expr] -> CompilationEnv)

initialState :: CompilationState
initialState = CompilationState {primitiveEnv}
  where
    primitiveEnv = primitives

compileAll :: Expr -> CompilationResult
compileAll p = (<> [ret]) . (compilePrologue <>) <$> evalState (compile p) initialState

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
  compiler <- gets (Map.lookup rator . primitiveEnv)
  case compiler of
    Nothing -> return $ Left (CompileError $ "Unknown operator: " <> rator)
    Just p -> compilePrimCall rator p rands
compile _ = return $ Right [nop]

compilePrimCall :: T.Text -> Primitive -> [Expr] -> CompilationEnv
compilePrimCall rator (Primitive arity f) rands
  | length rands /= arity =
      return $ Left (CompileError $ "Wrong number of arguments to " <> rator)
  | otherwise = f rands

unaryPrimCall :: T.Text -> [Expr] -> Program -> CompilationEnv
unaryPrimCall _ [rand] p = do
  prologue <- compile rand
  return $ (<> p) <$> prologue
unaryPrimCall name _ _ =
  return $
    Left (CompileError $ name <> ": invalid invocation")

compileFxadd1 :: [Expr] -> CompilationEnv
compileFxadd1 rands = unaryPrimCall "compileFxadd1" rands [add (FixI 1) RAX]

compileFxsub1 :: [Expr] -> CompilationEnv
compileFxsub1 rands = unaryPrimCall "compileFxsub1" rands [sub (FixI 1) RAX]

compileFixnumToChar :: [Expr] -> CompilationEnv
compileFixnumToChar rands = unaryPrimCall "compileFixnumToChar" rands p
  where
    p = [shl i RAX, Asm.or tag RAX]
    i = int $ charShift - fxShift
    tag = int charTag

compileCharToFixnum :: [Expr] -> CompilationEnv
compileCharToFixnum rands = unaryPrimCall "compileCharToFixnum" rands [shr i RAX]
  where
    i = int $ charShift - fxShift

boolCmp :: Program
boolCmp =
  [ sete AL, -- set al to 1 if equal
    movzb AL RAX, -- extend al to fill rax
    shl i RAX, -- shift the result to the T/F discriminant bit
    Asm.or f AL -- fill the leading bits with bool prefix
  ]
  where
    i = int (6 :: Int)
    f = int (0x2F :: Int)

compileFixnumP :: [Expr] -> CompilationEnv
compileFixnumP rands = unaryPrimCall "compileFixnumP" rands p
  where
    p = [Asm.and mask RAX, cmp tag RAX] <> boolCmp
    mask = int fxMask
    tag = int fxTag

primitives :: Env Primitive
primitives =
  Map.fromList
    [ ("fxadd1", Primitive 1 compileFxadd1),
      ("fxsub1", Primitive 1 compileFxsub1),
      ("fixnum->char", Primitive 1 compileFixnumToChar),
      ("char->fixnum", Primitive 1 compileCharToFixnum),
      ("fixnum?", Primitive 1 compileFixnumP)
    ]
