{-# LANGUAGE OverloadedStrings #-}

module Eval.PrimCall.UnarySpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO getTestFiles
  describe "Eval unary primitive calls" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "Evaluates fxadd1" $ do
      "(fxadd1 0)" `shouldPrint` "1\n"
      "(fxadd1 -1)" `shouldPrint` "0\n"
      "(fxadd1 1)" `shouldPrint` "2\n"
      "(fxadd1 -100)" `shouldPrint` "-99\n"
      "(fxadd1 1000)" `shouldPrint` "1001\n"
      "(fxadd1 536870910)" `shouldPrint` "536870911\n"
      "(fxadd1 -536870912)" `shouldPrint` "-536870911\n"
      "(fxadd1 (fxadd1 0))" `shouldPrint` "2\n"
      "(fxadd1 (fxadd1 (fxadd1 (fxadd1 (fxadd1 (fxadd1 12))))))" `shouldPrint` "18\n"
