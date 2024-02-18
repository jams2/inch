{-# LANGUAGE OverloadedStrings #-}

module Eval.LetrecSpec (spec) where

import Eval.Util
import Test.Hspec

spec :: Spec
spec = do
  (runtime, asm, bin) <- runIO (getTestFiles "letrec")
  describe "`letrec' expressions" $ do
    let shouldPrint = shouldPrint' runtime asm bin
    it "compiles and runs letrec expressions" $ do
      -- "(letrec () 12)" `shouldPrint` "12\n"
      -- "(letrec () (let ((x 5)) (fx+ x x)))" `shouldPrint` "10\n"
      -- "(letrec ((f (λ () 5))) 7)" `shouldPrint` "7\n"
      -- "(letrec ((f (λ () 5))) (let ((x 12)) x))" `shouldPrint` "12\n"
      -- "(letrec ((f (λ () 5))) (f))" `shouldPrint` "5\n"
      -- "(letrec ((f (λ () 5))) (let ((x (f))) x))" `shouldPrint` "5\n"
      -- "(letrec ((f (λ () 5))) (fx+ (f) 6))" `shouldPrint` "11\n"
      -- "(letrec ((f (λ () 5))) (fx+ 6 (f)))" `shouldPrint` "11\n"
      -- "(letrec ((f (λ () 5))) (fx- 20 (f)))" `shouldPrint` "15\n"
      -- "(letrec ((f (λ () 5))) (fx+ (f) (f)))" `shouldPrint` "10\n"
      -- "(letrec ((f (λ () (fx+ 5 7))) (g (λ () 13))) (fx+ (f) (g)))"
      --   `shouldPrint` "25\n"
      -- "(letrec ((f (λ (x) (fx+ x 12)))) (f 13))" `shouldPrint` "25\n"
      -- "(letrec ((f (λ (x) (fx+ x 12)))) (f (f 10)))" `shouldPrint` "34\n"
      -- "(letrec ((f (λ (x) (fx+ x 12)))) (f (f (f 0))))" `shouldPrint` "36\n"
      -- "(letrec ((f (λ (x y) (fx+ x y))) (g (λ (x) (fx+ x 12)))) (f 16 (f (g 0) (fx+ 1 (g 0)))))"
        -- `shouldPrint` "41\n"
      "(letrec ((f (λ (x) (g x x))) (g (λ (x y) (fx+ x y)))) (f 12))"
        `shouldPrint` "24\n"
      -- "(letrec ((f (λ (x) (if (fxzero? x) 1 (fx* x (f (fxsub1 x))))))) (f 5))"
      --   `shouldPrint` "120\n"
      -- "(letrec ((f (λ (x acc) (if (fxzero? x) acc (f (fxsub1 x) (fx* acc x)))))) (f 5 1))"
      --   `shouldPrint` "120\n"
      -- "(letrec ((f (λ (x) (if (fxzero? x) 0 (fx+ 1 (f (fxsub1 x))))))) (f 200))"
      --   `shouldPrint` "200\n"
