{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile
  ( functionHeader,
    prologue,
    compile,
    compileAll,
    compileError,
  )
where

import AST
import Arch
import Asm
import Constants qualified as C
import Control.Monad.State.Lazy
import Data.Either
import Data.HashMap.Strict qualified as Map
import Data.List (foldl')
import Data.Text qualified as T
import Prelude hiding (and, or)

initialStackIndex :: Int
initialStackIndex = -C.wordSize

newtype ErrorGroup a = ErrorGroup [a] deriving (Show, Functor)

type CompileError = ErrorGroup T.Text

foldErrors :: [CompileError] -> CompileError
foldErrors = ErrorGroup . foldl' (\acc (ErrorGroup es) -> acc <> es) []

compileError :: a -> ErrorGroup a
compileError msg = ErrorGroup $ pure msg

type Result = Either CompileError Program

type Env a = Map.HashMap Symbol a

data EnvMember = EnvStackIndex Int | EnvLabel T.Text

type LexicalEnv = Env EnvMember

type StackIndex = Int

data CompilationEnv = CompilationEnv
  { primitiveEnv :: !(Env Primitive),
    counter :: !Int,
    functions :: !Program
  }

type CompilationState a = State CompilationEnv a

type Compiler a = StackIndex -> LexicalEnv -> CompilationState a

-- | arity, compiler
data Primitive = Primitive !Int !([Expr] -> Compiler Result)

initialState :: CompilationEnv
initialState = CompilationEnv {primitiveEnv, counter, functions}
  where
    primitiveEnv = primitives
    counter = 0
    functions = []

getLabel :: CompilationState T.Text
getLabel = do
  i <- gets counter
  modify' increment
  return $ "L_" <> T.pack (show i)
  where
    increment x = x {counter = 1 + counter x}

nextStackIndex :: StackIndex -> StackIndex
nextStackIndex i = i - C.wordSize

collectResults :: [Result] -> Result
collectResults rs = case partitionEithers rs of
  ([], chunks) -> Right $ concat chunks
  (errors, _) -> Left $ foldErrors errors

functionHeader :: T.Text -> Program
functionHeader name =
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
prologue :: Program
prologue =
  functionHeader "scheme_entry"
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
    <> functionHeader "L_scheme_entry"

compileAll :: Expr -> Result
compileAll p =
  let (result, finalState) =
        runState (compile (desugar p) initialStackIndex Map.empty) initialState
   in case result of
        err@(Left _) -> err
        Right p' -> Right $ functions finalState ++ prologue ++ p' ++ [ret]

compile :: Expr -> Compiler Result
compile = \cases
  NilExpr _ _ -> return $ Right [mov NilI RAX]
  TrueExpr _ _ -> return $ Right [mov TrueI RAX]
  FalseExpr _ _ -> return $ Right [mov FalseI RAX]
  (FixnumExpr stackIndex) _ _ -> return $ Right [mov (FixI stackIndex) RAX]
  (CharExpr c) _ _ -> return $ Right [mov (CharI c) RAX]
  (LetrecExpr binders body) stackIndex env -> compileLetrec binders body stackIndex env
  (IfExpr t c a) stackIndex env -> compileIf t c a stackIndex env
  (LetExpr binders body) stackIndex env -> compileLet binders body stackIndex env
  (AppExpr (SymbolExpr rator) rands) stackIndex env ->
    compileApp rator rands stackIndex env
  (SymbolExpr name) stackIndex env -> compileLookup name stackIndex env
  _ _ _ -> return $ Right [nop]

data Binder = Binder !T.Text !Expr !T.Text -- name, lambda, label

getBinder :: Expr -> CompilationState (Either CompileError Binder)
getBinder (ListExpr [SymbolExpr name, lambda]) = Right . Binder name lambda <$> getLabel
getBinder p =
  return $
    Left (compileError $ "Invalid binder form in letrec: " <> T.pack (show p))

compileLetrec :: [Expr] -> Expr -> Compiler Result
compileLetrec binders body stackIndex env = do
  binders' <- mapM getBinder binders
  case partitionEithers binders' of
    (e : es, _) -> return $ Left (foldErrors (e : es))
    ([], bs) -> do
      let env' =
            foldl'
              (\e (Binder name _ label) -> Map.insert name (EnvLabel label) e)
              env
              bs
      ie <- loop bs stackIndex env'
      case ie of
        Left err -> return $ Left err
        Right (i, e) -> compile body i e
  where
    loop :: [Binder] -> Compiler (Either CompileError (StackIndex, LexicalEnv))
    loop [] stackIndex' env' = return $ Right (stackIndex', env')
    loop (Binder name lambda label : rest) stackIndex' env' = do
      p1 <- compileLambda lambda env'
      case p1 of
        Left err -> return $ Left err
        Right p -> do
          -- collect function defs as we go, they will be prepended to the rest of
          -- the generated code
          modify' (\s -> s {functions = functions s ++ functionHeader label ++ p})
          loop rest stackIndex' (Map.insert name (EnvLabel label) env')

bindLabels :: [Expr] -> LexicalEnv -> CompilationState (Either CompileError LexicalEnv)
bindLabels [] env = return $ Right env
bindLabels (ListExpr [SymbolExpr name, _] : rest) env = do
  label <- getLabel
  bindLabels rest (Map.insert name (EnvLabel label) env)
bindLabels p _ =
  return $
    Left (compileError $ "Invalid binding form in letrec: " <> T.pack (show p))

compileLambda :: Expr -> LexicalEnv -> CompilationState Result
compileLambda (LambdaExpr args body) env = do
  let nextEnv =
        foldl' (\env' (name, i) -> Map.insert name (EnvStackIndex i) env') env $
          zip [n | SymbolExpr n <- args] [-C.wordSize, -2 * C.wordSize ..]
  -- args stored on stack with one word left for the return address
  fmap (<> [ret]) <$> compile body (-(length args + 1) * C.wordSize) nextEnv
compileLambda p _ = return $ Left (compileError $ "Expected LambdaExpr, got: " <> T.pack (show p))

compileApp :: Symbol -> [Expr] -> Compiler Result
compileApp rator rands stackIndex env = do
  compiler <- gets (Map.lookup rator . primitiveEnv)
  case compiler of
    Nothing -> case Map.lookup rator env of
      Just (EnvLabel label) -> compileFuncall label rands stackIndex env
      _ -> return $ Left (compileError $ "Unknown operator: " <> rator)
    Just p -> compilePrimCall rator p rands stackIndex env

compileFuncall :: T.Text -> [Expr] -> Compiler Result
compileFuncall label rands stackIndex env =
  fmap (++ funcall) <$> loop rands (nextStackIndex stackIndex) env
  where
    funcall =
      -- Adjust the stack pointer - take into account current offset, and the fact that
      -- the x86 call instruction causes the return address to be pushed - we set
      -- the stack pointer to one word before the current stackIndex, which we left
      -- empty when compiling the args so nothing gets clobbered.
      [ add (int (stackIndex + C.wordSize)) RSP,
        call label,
        -- Restore the stack pointer.
        sub (int (stackIndex + C.wordSize)) RSP
      ]
    loop :: [Expr] -> Compiler Result
    loop (x : xs) i e = do
      p <- withStackSave x i e
      case p of
        err@(Left _) -> return err
        Right p' -> fmap (p' <>) <$> loop xs (nextStackIndex i) e
    loop _ _ _ = return $ Right []

compileLet :: [Expr] -> Expr -> Compiler Result
compileLet binders body stackIndex env = do
  bindings <- loop binders env stackIndex env
  case bindings of
    Left err -> return $ Left err
    Right (stackIndex', env', p) -> do
      body' <- compile body stackIndex' env'
      return $ (p <>) <$> body'
  where
    loop ::
      [Expr] ->
      LexicalEnv ->
      Compiler (Either CompileError (StackIndex, LexicalEnv, Program))
    loop [] _ stackIndex' env' = return $ Right (stackIndex', env', [])
    loop (ListExpr [SymbolExpr name, rhs] : rest) initialEnv stackIndex' nextEnv = do
      rhs' <- withStackSave rhs stackIndex' initialEnv
      case rhs' of
        (Left err) -> return $ Left err
        (Right p') -> do
          next <-
            loop
              rest
              initialEnv
              (nextStackIndex stackIndex')
              (Map.insert name (EnvStackIndex stackIndex') nextEnv)
          return $ fmap (fmap (p' <>)) next
    loop _ _ _ _ = return $ Left (compileError "")

compileLookup :: Symbol -> Compiler Result
compileLookup name _ env = do
  case Map.lookup name env of
    Just (EnvStackIndex stackIndex) -> return $ Right [mov (stackIndex % RSP) RAX]
    _ -> return $ Left (compileError ("Unbound variable: " <> name))

compileIf :: Expr -> Expr -> Expr -> Compiler Result
compileIf t c a stackIndex env = do
  altLabel <- getLabel
  endLabel <- getLabel
  chunks <-
    sequence
      [ compile t stackIndex env,
        return $ Right [cmp (int C.false) AL, je altLabel],
        compile c stackIndex env,
        return $ Right [jmp endLabel, Label altLabel],
        compile a stackIndex env,
        return $ Right [Label endLabel]
      ]
  return $ collectResults chunks

compilePrimCall :: T.Text -> Primitive -> [Expr] -> Compiler Result
compilePrimCall rator (Primitive arity f) rands stackIndex env
  | length rands /= arity =
      return $ Left (compileError $ "Wrong number of arguments to " <> rator)
  | otherwise = f rands stackIndex env

unaryPrimCall :: T.Text -> [Expr] -> Program -> Compiler Result
unaryPrimCall = \cases
  _ [rand] body stackIndex env ->
    fmap (<> body) <$> compile rand stackIndex env
  name _ _ _ _ ->
    return $
      Left (compileError $ name <> ": invalid invocation")

binaryPrimCall :: T.Text -> [Expr] -> Program -> Compiler Result
binaryPrimCall = \cases
  _ [x, y] body stackIndex env ->
    fmap (<> body) <$> compileBinArgs x y stackIndex env
  name _ _ _ _ ->
    return $
      Left (compileError $ name <> ": invalid invocation")

withStackSave :: Expr -> Compiler Result
withStackSave p stackIndex env =
  fmap (<> [mov RAX (stackIndex % RSP)]) <$> compile p stackIndex env

compileBinArgs :: Expr -> Expr -> Compiler Result
compileBinArgs x y stackIndex env = do
  x' <- withStackSave x stackIndex env
  y' <- compile y (nextStackIndex stackIndex) env
  return $ collectResults [x', y']

compileFxadd1 :: [Expr] -> Compiler Result
compileFxadd1 rands = unaryPrimCall "compileFxadd1" rands [add (FixI 1) RAX]

compileFxsub1 :: [Expr] -> Compiler Result
compileFxsub1 rands = unaryPrimCall "compileFxsub1" rands [sub (FixI 1) RAX]

compileFixnumToChar :: [Expr] -> Compiler Result
compileFixnumToChar rands = unaryPrimCall "compileFixnumToChar" rands p
  where
    p = [shl i RAX, or tag RAX]
    i = int $ C.charShift - C.fxShift
    tag = int C.charTag

compileCharToFixnum :: [Expr] -> Compiler Result
compileCharToFixnum rands = unaryPrimCall "compileCharToFixnum" rands [shr i RAX]
  where
    i = int $ C.charShift - C.fxShift

boolCmp :: (Register -> Line) -> Program
boolCmp op =
  [ op AL, -- set al to 1 if comparison succeeds
    movzb AL RAX, -- extend al to fill rax
    shl i RAX, -- shift the result to the T/F discriminant bit
    or f AL -- fill the leading bits with bool prefix
  ]
  where
    i = int (6 :: Int)
    f = int (0x2F :: Int)

compileFixnumP :: [Expr] -> Compiler Result
compileFixnumP rands = unaryPrimCall "compileFixnumP" rands p
  where
    p = [and mask RAX, cmp tag RAX] ++ boolCmp sete
    mask = int C.fxMask
    tag = int C.fxTag

compileFxzeroP :: [Expr] -> Compiler Result
compileFxzeroP rands = unaryPrimCall "compileFxzeroP" rands p
  where
    p = cmp (int (0 :: Int)) RAX : boolCmp sete

compileNullP :: [Expr] -> Compiler Result
compileNullP rands = unaryPrimCall "compileNullP" rands p
  where
    p = cmp (int C.nil) RAX : boolCmp sete

compileBooleanP :: [Expr] -> Compiler Result
compileBooleanP rands = unaryPrimCall "compileBooleanP" rands p
  where
    p =
      [ and (int C.boolMask) AL, -- F & F and F & T both evaluate to F
        cmp (int C.false) AL
      ]
        ++ boolCmp sete

compileCharP :: [Expr] -> Compiler Result
compileCharP rands = unaryPrimCall "compileCharP" rands p
  where
    p =
      [ and (int C.charMask) AL,
        cmp (int C.charTag) AL
      ]
        ++ boolCmp sete

compileNot :: [Expr] -> Compiler Result
compileNot rands = unaryPrimCall "compileNot" rands p
  where
    p = cmp (int C.false) AL : boolCmp sete

compileFxLogNot :: [Expr] -> Compiler Result
compileFxLogNot rands = unaryPrimCall "compileFxLogNot" rands p
  where
    p =
      [ Asm.not RAX,
        and (int (0xFC :: Int)) AL -- reset the fixnum tag
      ]

compileFxPlus :: [Expr] -> Compiler Result
compileFxPlus rands i = binaryPrimCall "compileFxPlus" rands [add (i % RSP) RAX] i

compileFxMinus :: [Expr] -> Compiler Result
compileFxMinus rands i =
  binaryPrimCall
    "compileFxMinus"
    rands
    [ sub RAX (i % RSP),
      mov (i % RSP) RAX
    ]
    i

{-
Input fixnums are scaled by 4. This means naive multiplication will do
4x * 4y = 16xy, where we want 4xy.

Thus we implement multiplication as 4xy = (4x / 4) * 4y, using sar to
implement the division.
-}
compileFxMul :: [Expr] -> Compiler Result
compileFxMul rands i =
  binaryPrimCall
    "compileFxMul"
    rands
    [ sar (int C.fxShift) (i % RSP),
      imul (i % RSP) RAX
    ]
    i

compileFxLogAnd :: [Expr] -> Compiler Result
compileFxLogAnd rands i =
  binaryPrimCall "compileFxLogAnd" rands [and (i % RSP) RAX] i

compileFxLogOr :: [Expr] -> Compiler Result
compileFxLogOr rands i =
  binaryPrimCall
    "compileFxLogOr"
    rands
    [or (i % RSP) RAX]
    i

compileFxCmp :: T.Text -> (Register -> Line) -> [Expr] -> Compiler Result
compileFxCmp name cmp' rands i =
  binaryPrimCall
    name
    rands
    (cmp RAX (i % RSP) : boolCmp cmp')
    i

compileFxEq :: [Expr] -> Compiler Result
compileFxEq = compileFxCmp "compileFxEq" sete

compileFxLt :: [Expr] -> Compiler Result
compileFxLt = compileFxCmp "compileFxLt" setl

compileFxLte :: [Expr] -> Compiler Result
compileFxLte = compileFxCmp "compileFxLte" setle

compileFxGt :: [Expr] -> Compiler Result
compileFxGt = compileFxCmp "compileFxGt" setg

compileFxGte :: [Expr] -> Compiler Result
compileFxGte = compileFxCmp "compileFxGte" setge

compileCharEq :: [Expr] -> Compiler Result
compileCharEq = compileFxCmp "compileCharEq" sete

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
      ("fxlognot", Primitive 1 compileFxLogNot),
      ("fx+", Primitive 2 compileFxPlus),
      ("fx-", Primitive 2 compileFxMinus),
      ("fx*", Primitive 2 compileFxMul),
      ("fxlogand", Primitive 2 compileFxLogAnd),
      ("fxlogor", Primitive 2 compileFxLogOr),
      ("fx=", Primitive 2 compileFxEq),
      ("fx<", Primitive 2 compileFxLt),
      ("fx<=", Primitive 2 compileFxLte),
      ("fx>", Primitive 2 compileFxGt),
      ("fx>=", Primitive 2 compileFxGte),
      ("char=", Primitive 2 compileCharEq)
      -- TODO: implement char=, char<, char<=, etc.
    ]
