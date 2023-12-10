{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( space,
    symbol,
    immediate,
    fixnum,
    stringLiteral,
    charLiteral,
    word,
    expr,
    parens,
  )
where

import AST
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

space :: Parser ()
space = L.space C.space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

word :: T.Text -> Parser T.Text
word = L.symbol space

parens :: Parser a -> Parser a
parens = between (word "(") (word ")")

charLiteral :: Parser Expr
charLiteral = AST.Char <$> lexeme (C.string "#\\" *> L.charLiteral) <?> "character literal"

stringLiteral :: Parser Expr
stringLiteral =
  String . T.pack
    <$> lexeme (C.char '\"' *> manyTill L.charLiteral (C.char '\"'))
    <?> "string literal"

fixnum :: Parser Expr
fixnum = Fixnum <$> lexeme L.decimal <?> "fixnum"

immediate :: Parser Expr
immediate =
  choice
    [ charLiteral,
      stringLiteral,
      fixnum
    ]

nil :: Parser Expr
nil = Nil <$ lexeme (C.string "nil")

identChars :: [Char]
identChars = "-<>?=."

symbol :: Parser Expr
symbol =
  Symbol . T.pack
    <$> lexeme
      ( some $
          choice
            [ C.letterChar,
              C.digitChar,
              satisfy (`elem` identChars)
            ]
      )
    <?> "symbol"

lambda :: Parser Expr
lambda = do
  _ <- word "("
  _ <- lexeme (C.string "lambda" <|> C.string "λ")
  args <- parens $ many symbol
  body <- expr
  _ <- word ")"
  return $ Lambda args body

app :: Parser Expr
app = do
  _ <- word "("
  rator <- expr
  rands <- many expr
  _ <- word ")"
  return $ App rator rands

expr :: Parser Expr
expr =
  choice
    [ nil,
      immediate,
      symbol,
      try lambda <|> app
    ]
