{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile
  ( compileFunctionHeader,
    compilePrologue,
    compile,
    compileAll,
  )
where

import AST
import Arch
import Asm
import Constants qualified as C
import Control.Monad.State.Lazy
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T

newtype CompileError = CompileError T.Text deriving (Show)

type Result = Either CompileError Program

type Env a = Map.HashMap T.Text a

data CompilationState = CompilationState
  { primitiveEnv :: !(Env Primitive),
    counter :: !Int
  }

type Compiler a = State CompilationState a

-- | arity, compiler
data Primitive = Primitive !Int !([Expr] -> Compiler Result)

initialState :: CompilationState
initialState = CompilationState {primitiveEnv, counter}
  where
    primitiveEnv = primitives
    counter = 0

getLabel :: Compiler T.Text
getLabel = do
  i <- gets counter
  modify' increment
  return $ "L_" <> T.pack (show i)
  where
    increment x = x {counter = 1 + counter x}

compileAll :: Expr -> Result
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

compile :: Expr -> Compiler Result
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

compilePrimCall :: T.Text -> Primitive -> [Expr] -> Compiler Result
compilePrimCall rator (Primitive arity f) rands
  | length rands /= arity =
      return $ Left (CompileError $ "Wrong number of arguments to " <> rator)
  | otherwise = f rands

unaryPrimCall :: T.Text -> [Expr] -> Program -> Compiler Result
unaryPrimCall _ [rand] p = do
  prologue <- compile rand
  return $ (<> p) <$> prologue
unaryPrimCall name _ _ =
  return $
    Left (CompileError $ name <> ": invalid invocation")

compileFxadd1 :: [Expr] -> Compiler Result
compileFxadd1 rands = unaryPrimCall "compileFxadd1" rands [add (FixI 1) RAX]

compileFxsub1 :: [Expr] -> Compiler Result
compileFxsub1 rands = unaryPrimCall "compileFxsub1" rands [sub (FixI 1) RAX]

compileFixnumToChar :: [Expr] -> Compiler Result
compileFixnumToChar rands = unaryPrimCall "compileFixnumToChar" rands p
  where
    p = [shl i RAX, Asm.or tag RAX]
    i = int $ C.charShift - C.fxShift
    tag = int C.charTag

compileCharToFixnum :: [Expr] -> Compiler Result
compileCharToFixnum rands = unaryPrimCall "compileCharToFixnum" rands [shr i RAX]
  where
    i = int $ C.charShift - C.fxShift

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

compileFixnumP :: [Expr] -> Compiler Result
compileFixnumP rands = unaryPrimCall "compileFixnumP" rands p
  where
    p = [Asm.and mask RAX, cmp tag RAX] <> boolCmp
    mask = int C.fxMask
    tag = int C.fxTag

compileFxzeroP :: [Expr] -> Compiler Result
compileFxzeroP rands = unaryPrimCall "compileFxzeroP" rands p
  where
    p = [cmp (int (0 :: Int)) RAX] <> boolCmp

compileNullP :: [Expr] -> Compiler Result
compileNullP rands = unaryPrimCall "compileNullP" rands p
  where
    p = [cmp (int C.nil) RAX] <> boolCmp

compileBooleanP :: [Expr] -> Compiler Result
compileBooleanP rands = unaryPrimCall "compileBooleanP" rands p
  where
    p =
      [ Asm.and (int C.boolMask) AL, -- F & F and F & T both evaluate to F
        cmp (int C.false) AL
      ]
        <> boolCmp

compileCharP :: [Expr] -> Compiler Result
compileCharP rands = unaryPrimCall "compileCharP" rands p
  where
    p =
      [ Asm.and (int C.charMask) AL,
        cmp (int C.charTag) AL
      ]
        <> boolCmp

compileNot :: [Expr] -> Compiler Result
compileNot rands = unaryPrimCall "compileNot" rands p
  where
    p = [cmp (int C.false) AL] <> boolCmp

compileFxlognot :: [Expr] -> Compiler Result
compileFxlognot rands = unaryPrimCall "compileFxlognot" rands p
  where
    p =
      [ Asm.not RAX,
        Asm.and (int (0xFC :: Int)) AL -- reset the fixnum tag
      ]

primitives :: Env Primitive
primitives =
  Map.fromList
    [ ("fxadd1", Primitive 1 compileFxadd1),
      ("fxsub1", Primitive 1 compileFxsub1),
      ("fixnum->char", Primitive 1 compileFixnumToChar),
      ("char->fixnum", Primitive 1 compileCharToFixnum),
      ("fixnum?", Primitive 1 compileFixnumP),
      ("fxzero?", Primitive 1 compileFxzeroP),
      ("null?", Primitive 1 compileNullP),
      ("boolean?", Primitive 1 compileBooleanP),
      ("char?", Primitive 1 compileCharP),
      ("not", Primitive 1 compileNot),
      ("fxlognot", Primitive 1 compileFxlognot)
    ]
