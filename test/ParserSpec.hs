{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import AST
import Parser
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = describe "Parser" $ do
  it "can parse fixnums" $ do
    parseMaybe expr "1" `shouldBe` Just (Fixnum 1)
  it "can parse char literals" $ do
    parseMaybe expr "#\\a" `shouldBe` Just (Char 'a')
  it "can parse string literals" $ do
    parseMaybe expr "\"foo bar baz\"" `shouldBe` Just (String "foo bar baz")
    parseMaybe expr "\"foo \\\"bar baz\"" `shouldBe` Just (String "foo \"bar baz")
  it "can parse symbols" $ do
    parseMaybe expr "->foo" `shouldBe` Just (Symbol "->foo")
    parseMaybe expr "foo" `shouldBe` Just (Symbol "foo")
  it "can parse applications" $ do
    parseMaybe expr "(foo)" `shouldBe` Just (App (Symbol "foo") [])
    parseMaybe expr "(foo bar)" `shouldBe` Just (App (Symbol "foo") [Symbol "bar"])
    parseMaybe expr "(foo bar baz)"
      `shouldBe` Just (App (Symbol "foo") [Symbol "bar", Symbol "baz"])
    parseMaybe expr "((foo) (bar baz))"
      `shouldBe` Just
        ( App
            (App (Symbol "foo") [])
            [App (Symbol "bar") [Symbol "baz"]]
        )
  it "can parse nil" $ do
    parseMaybe expr "nil" `shouldBe` Just Nil
  it "can parse lambdas" $ do
    parseMaybe expr "(lambda (x) x)" `shouldBe` Just (Lambda [Symbol "x"] (Symbol "x"))
    parseMaybe expr "(λ (x) x)" `shouldBe` Just (Lambda [Symbol "x"] (Symbol "x"))
    parseMaybe expr "(λ (x y) x)" `shouldBe` Just (Lambda [Symbol "x", Symbol "y"] (Symbol "x"))
