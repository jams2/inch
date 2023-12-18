{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import AST
import Parser
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = describe "Parser" $ do
  it "can parse fixnums" $ do
    parseMaybe expr "1" `shouldBe` Just (FixnumExpr 1)
    parseMaybe expr "-1" `shouldBe` Just (FixnumExpr (-1))
  it "can parse char literals" $ do
    parseMaybe expr "#\\a" `shouldBe` Just (CharExpr 'a')
  it "can parse string literals" $ do
    parseMaybe expr "\"foo bar baz\"" `shouldBe` Just (StringExpr "foo bar baz")
    parseMaybe expr "\"foo \\\"bar baz\"" `shouldBe` Just (StringExpr "foo \"bar baz")
  it "can parse symbols" $ do
    parseMaybe expr "->foo" `shouldBe` Just (SymbolExpr "->foo")
    parseMaybe expr "foo" `shouldBe` Just (SymbolExpr "foo")
  it "can parse applications" $ do
    parseMaybe expr "(foo)" `shouldBe` Just (AppExpr (SymbolExpr "foo") [])
    parseMaybe expr "(foo bar)"
      `shouldBe` Just
        ( AppExpr
            (SymbolExpr "foo")
            [SymbolExpr "bar"]
        )
    parseMaybe expr "(foo bar baz)"
      `shouldBe` Just (AppExpr (SymbolExpr "foo") [SymbolExpr "bar", SymbolExpr "baz"])
    parseMaybe expr "((foo) (bar baz))"
      `shouldBe` Just
        ( AppExpr
            (AppExpr (SymbolExpr "foo") [])
            [AppExpr (SymbolExpr "bar") [SymbolExpr "baz"]]
        )
  it "can parse nil" $ do
    parseMaybe expr "nil" `shouldBe` Just NilExpr
  it "can parse lambdas" $ do
    parseMaybe expr "(lambda (x) x)"
      `shouldBe` Just
        ( LambdaExpr
            [SymbolExpr "x"]
            (SymbolExpr "x")
        )
    parseMaybe expr "(λ (x) x)"
      `shouldBe` Just
        ( LambdaExpr
            [SymbolExpr "x"]
            (SymbolExpr "x")
        )
    parseMaybe expr "(λ (x y) x)"
      `shouldBe` Just
        (LambdaExpr [SymbolExpr "x", SymbolExpr "y"] (SymbolExpr "x"))
  it "can parse bools" $ do
    parseMaybe expr "#t" `shouldBe` Just TrueExpr
    parseMaybe expr "#f" `shouldBe` Just FalseExpr
