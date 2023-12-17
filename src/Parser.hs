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
import Control.Monad.Combinators
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
fixnum = Fixnum <$> (L.signed space integer <?> "fixnum")
  where
    integer = lexeme L.decimal

nil :: Parser Expr
nil = Nil <$ lexeme (C.string "nil")

bool :: Parser Expr
bool = Bool <$> (True <$ lexeme (C.string "#t") <|> False <$ lexeme (C.string "#f"))

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

immediate :: Parser Expr
immediate =
  choice
    [ nil,
      bool,
      charLiteral,
      stringLiteral,
      try fixnum <|> symbol
    ]

identChars :: [Char]
identChars = "-<>?=."

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
    [ immediate,
      try lambda <|> app
    ]
