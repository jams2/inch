{-# LANGUAGE OverloadedStrings #-}

module Eval.ConstantSpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO getTestFiles
  describe "Eval" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "Evaluates constants" $ do
      "nil" `shouldPrint` "()\n"
      "#t" `shouldPrint` "#t\n"
      "#f" `shouldPrint` "#f\n"
    it "Evaluates fixnums" $ do
      "0" `shouldPrint` "0\n"
      "1" `shouldPrint` "1\n"
      "-1" `shouldPrint` "-1\n"
      "10" `shouldPrint` "10\n"
      "-10" `shouldPrint` "-10\n"
      "2736" `shouldPrint` "2736\n"
      "-2736" `shouldPrint` "-2736\n"
      "536870911" `shouldPrint` "536870911\n"
      "-536870912" `shouldPrint` "-536870912\n"
