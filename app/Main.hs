{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Text.IO qualified as T
import Compile
import Emit
import Parser
import System.IO
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  parseResult <- parse expr "<stdin>" <$> T.hGetContents stdin
  case parseResult of
    Left err -> putStr (errorBundlePretty err)
    Right ast -> case compileAll ast of
      Left err -> print err
      Right program -> emit stdout program
