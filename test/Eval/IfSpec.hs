{-# LANGUAGE OverloadedStrings #-}

module Eval.IfSpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO (getTestFiles "if")
  describe "`if' expressions" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "compiles and runs `if' expressions" $ do
      "(if #t 12 13)" `shouldPrint` "12\n"
      "(if #f 12 13)" `shouldPrint` "13\n"
      "(if 0 12 13)" `shouldPrint` "12\n"
      "(if () 43 ())" `shouldPrint` "43\n"
      "(if #t (if 12 13 4) 17)" `shouldPrint` "13\n"
      "(if #f 12 (if #f 13 4))" `shouldPrint` "4\n"
      "(if #\\X (if 1 2 3) (if 4 5 6))" `shouldPrint` "2\n"
      "(if (not (boolean? #t)) 15 (boolean? #f))" `shouldPrint` "#t\n"
      "(if (if (char? #\\a) (boolean? #\\b) (fixnum? #\\c)) 119 -23)" `shouldPrint` "-23\n"
      "(if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7)" `shouldPrint` "6\n"
      "(if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7)" `shouldPrint` "7\n"
      "(not (if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7))" `shouldPrint` "#f\n"
      "(if (char? 12) 13 14)" `shouldPrint` "14\n"
      "(if (char? #\\a) 13 14)" `shouldPrint` "13\n"
      "(fxadd1 (if (fxsub1 1) (fxsub1 13) 14))" `shouldPrint` "13\n"
    it "desugars, compiles and runs `and' expressions" $ do
      "(and)" `shouldPrint` "#t\n"
      "(and #t)" `shouldPrint` "#t\n"
      "(and #f)" `shouldPrint` "#f\n"
      "(and #\\a)" `shouldPrint` "#\\a\n"
      "(and #t #f)" `shouldPrint` "#f\n"
      "(and (fixnum? 7) 42)" `shouldPrint` "42\n"
      "(and (or (fixnum? #f) #\\a) 42)" `shouldPrint` "42\n"
    it "desugars, compiles and runs `or' expressions" $ do
      "(or)" `shouldPrint` "#f\n"
      "(or 1)" `shouldPrint` "1\n"
