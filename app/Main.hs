{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Asm
import AST
import Data.Text.IO qualified as T
import Compile
import Emit
import IR
import Parser
import System.IO
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

compileAll :: Expr -> CompilationResult
compileAll expr = (compilePrologue <>) <$> compile (lower expr)

main :: IO ()
main = do
  parseResult <- parse expr "<stdin>" <$> T.hGetContents stdin
  case parseResult of
    Left err -> putStr (errorBundlePretty err)
    Right ast -> case compileAll ast of
      Left err -> putStrLn $ show err
      Right program -> emit stdout program
