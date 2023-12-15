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
