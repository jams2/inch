{-# LANGUAGE OverloadedStrings #-}

module Eval.PrimCall.BinarySpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO (getTestFiles "binary-primcall")
  describe "binary primitive calls" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "compiles and runs fx+" $ do
      "(fx+ 1 2)" `shouldPrint` "3\n"
      "(fx+ 1 -2)" `shouldPrint` "-1\n"
      "(fx+ -1 2)" `shouldPrint` "1\n"
      "(fx+ -1 -2)" `shouldPrint` "-3\n"
      "(fx+ 536870911 -1)" `shouldPrint` "536870910\n"
      "(fx+ 536870910 1)" `shouldPrint` "536870911\n"
      "(fx+ -536870912 1)" `shouldPrint` "-536870911\n"
      "(fx+ -536870911 -1)" `shouldPrint` "-536870912\n"
      "(fx+ 536870911 -536870912)" `shouldPrint` "-1\n"
      "(fx+ 1 (fx+ 2 3))" `shouldPrint` "6\n"
      "(fx+ 1 (fx+ 2 -3))" `shouldPrint` "0\n"
      "(fx+ 1 (fx+ -2 3))" `shouldPrint` "2\n"
      "(fx+ 1 (fx+ -2 -3))" `shouldPrint` "-4\n"
      "(fx+ -1 (fx+ 2 3))" `shouldPrint` "4\n"
      "(fx+ -1 (fx+ 2 -3))" `shouldPrint` "-2\n"
      "(fx+ -1 (fx+ -2 3))" `shouldPrint` "0\n"
      "(fx+ -1 (fx+ -2 -3))" `shouldPrint` "-6\n"
      "(fx+ (fx+ 1 2) 3)" `shouldPrint` "6\n"
      "(fx+ (fx+ 1 2) -3)" `shouldPrint` "0\n"
      "(fx+ (fx+ 1 -2) 3)" `shouldPrint` "2\n"
      "(fx+ (fx+ 1 -2) -3)" `shouldPrint` "-4\n"
      "(fx+ (fx+ -1 2) 3)" `shouldPrint` "4\n"
      "(fx+ (fx+ -1 2) -3)" `shouldPrint` "-2\n"
      "(fx+ (fx+ -1 -2) 3)" `shouldPrint` "0\n"
      "(fx+ (fx+ -1 -2) -3)" `shouldPrint` "-6\n"
      "(fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ 1 2) 3) 4) 5) 6) 7) 8) 9)" `shouldPrint` "45\n"
      "(fx+ 1 (fx+ 2 (fx+ 3 (fx+ 4 (fx+ 5 (fx+ 6 (fx+ 7 (fx+ 8 9))))))))" `shouldPrint` "45\n"
