{-# LANGUAGE OverloadedStrings #-}

module Eval.LetSpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO (getTestFiles "let")
  describe "`let' expressions" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "compiles and runs let expressions" $ do
      "(let ((x 5)) x)" `shouldPrint` "5\n"
      "(let ((x (fx+ 1 2))) x)" `shouldPrint` "3\n"
      "(let ((x (fx+ 1 2))) (let ((y (fx+ 3 4))) (fx+ x y)))" `shouldPrint` "10\n"
      "(let ((x (fx+ 1 2))) (let ((y (fx+ 3 4))) (fx- y x)))" `shouldPrint` "4\n"
      "(let ((x (fx+ 1 2)) (y (fx+ 3 4))) (fx- y x))" `shouldPrint` "4\n"
      "(let ((x (let ((y (fx+ 1 2))) (fx* y y)))) (fx+ x x))" `shouldPrint` "18\n"
      "(let ((x (fx+ 1 2))) (let ((x (fx+ 3 4))) x))" `shouldPrint` "7\n"
      "(let ((x (fx+ 1 2))) (let ((x (fx+ x 4))) x))" `shouldPrint` "7\n"
      "(let ((t (let ((t (let ((t (let ((t (fx+ 1 2))) t))) t))) t))) t)" `shouldPrint` "3\n"
      "(let ((x 12)) (let ((x (fx+ x x))) (let ((x (fx+ x x))) (let ((x (fx+ x x))) (fx+ x x)))))"
        `shouldPrint` "192\n"
