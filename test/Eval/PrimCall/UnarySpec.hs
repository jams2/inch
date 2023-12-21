{-# LANGUAGE OverloadedStrings #-}

module Eval.PrimCall.UnarySpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO (getTestFiles "unary-primcall")
  describe "Compile and run unary primitive calls" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "compiles and runs fxadd1" $ do
      "(fxadd1 0)" `shouldPrint` "1\n"
      "(fxadd1 -1)" `shouldPrint` "0\n"
      "(fxadd1 1)" `shouldPrint` "2\n"
      "(fxadd1 -100)" `shouldPrint` "-99\n"
      "(fxadd1 1000)" `shouldPrint` "1001\n"
      "(fxadd1 536870910)" `shouldPrint` "536870911\n"
      "(fxadd1 -536870912)" `shouldPrint` "-536870911\n"
      "(fxadd1 (fxadd1 0))" `shouldPrint` "2\n"
      "(fxadd1 (fxadd1 (fxadd1 (fxadd1 (fxadd1 (fxadd1 12))))))" `shouldPrint` "18\n"
    it "compiles and runs fxsub1" $ do
      "(fxsub1 0)" `shouldPrint` "-1\n"
      "(fxsub1 -1)" `shouldPrint` "-2\n"
      "(fxsub1 1)" `shouldPrint` "0\n"
      "(fxsub1 -100)" `shouldPrint` "-101\n"
      "(fxsub1 1000)" `shouldPrint` "999\n"
      "(fxsub1 536870911)" `shouldPrint` "536870910\n"
      "(fxsub1 -536870911)" `shouldPrint` "-536870912\n"
      "(fxsub1 (fxsub1 0))" `shouldPrint` "-2\n"
      "(fxsub1 (fxsub1 (fxsub1 (fxsub1 (fxsub1 (fxsub1 12))))))" `shouldPrint` "6\n"
      "(fxsub1 (fxadd1 0))" `shouldPrint` "0\n"
    it "compiles and runs fixnum->char" $ do
      "(fixnum->char 65)" `shouldPrint` "#\\A\n"
      "(fixnum->char 97)" `shouldPrint` "#\\a\n"
      "(fixnum->char 122)" `shouldPrint` "#\\z\n"
      "(fixnum->char 90)" `shouldPrint` "#\\Z\n"
      "(fixnum->char 48)" `shouldPrint` "#\\0\n"
      "(fixnum->char 57)" `shouldPrint` "#\\9\n"
      "(char->fixnum #\\A)" `shouldPrint` "65\n"
      "(char->fixnum #\\a)" `shouldPrint` "97\n"
      "(char->fixnum #\\z)" `shouldPrint` "122\n"
      "(char->fixnum #\\Z)" `shouldPrint` "90\n"
      "(char->fixnum #\\0)" `shouldPrint` "48\n"
      "(char->fixnum #\\9)" `shouldPrint` "57\n"
      "(char->fixnum (fixnum->char 12))" `shouldPrint` "12\n"
      "(fixnum->char (char->fixnum #\\x))" `shouldPrint` "#\\x\n"
    it "compiles and runs fixnum?" $ do
      "(fixnum? 0)" `shouldPrint` "#t\n"
      "(fixnum? 1)" `shouldPrint` "#t\n"
      "(fixnum? -1)" `shouldPrint` "#t\n"
      "(fixnum? 37287)" `shouldPrint` "#t\n"
      "(fixnum? -23873)" `shouldPrint` "#t\n"
      "(fixnum? 536870911)" `shouldPrint` "#t\n"
      "(fixnum? -536870912)" `shouldPrint` "#t\n"
      "(fixnum? #t)" `shouldPrint` "#f\n"
      "(fixnum? #f)" `shouldPrint` "#f\n"
      "(fixnum? nil)" `shouldPrint` "#f\n"
      "(fixnum? #\\Q)" `shouldPrint` "#f\n"
      "(fixnum? (fixnum? 12))" `shouldPrint` "#f\n"
      "(fixnum? (fixnum? #f))" `shouldPrint` "#f\n"
      "(fixnum? (fixnum? #\\A))" `shouldPrint` "#f\n"
      "(fixnum? (char->fixnum #\\r))" `shouldPrint` "#t\n"
      "(fixnum? (fixnum->char 12))" `shouldPrint` "#f\n"
    it "compiles and runs fxzero?" $ do
      "(fxzero? 0)" `shouldPrint` "#t\n"
      "(fxzero? 1)" `shouldPrint` "#f\n"
      "(fxzero? -1)" `shouldPrint` "#f\n"
      "(fxzero? 64)" `shouldPrint` "#f\n"
      "(fxzero? 960)" `shouldPrint` "#f\n"
