{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval.Util (compileAndRun, shouldPrint', getTestFiles) where

import Compile
import Data.Text qualified as T
import Emit
import Parser
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec
import Text.Megaparsec

compileAndRun :: FilePath -> FilePath -> FilePath -> T.Text -> IO String
compileAndRun runtime asmFile bin p = do
  let parseResult = parse expr "test" p
  case parseResult of
    Left err -> return (errorBundlePretty err)
    Right ast -> case compileAll ast of
      Left err -> return $ show err
      Right program -> do
        withFile asmFile WriteMode (`emit` program)
        callProcess "gcc" ["-fomit-frame-pointer", runtime, asmFile, "-o", bin]
        readProcess bin [] []

shouldPrint' :: FilePath -> FilePath -> FilePath -> T.Text -> String -> IO ()
shouldPrint' runtime asm bin p s = do
  out <- compileAndRun runtime asm bin p
  out `shouldBe` s

getTestFiles :: String -> IO (FilePath, FilePath, FilePath) -- startup.c, test.s, test exe
getTestFiles testName = do
  root <- getCurrentDirectory
  tmp <- getCanonicalTemporaryDirectory >>= (`createTempDirectory` ("inch-test--" ++ testName))
  return (root </> "startup.c", tmp </> "test.s", tmp </> "test")
