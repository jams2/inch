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
