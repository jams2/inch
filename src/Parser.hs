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
charLiteral = CharExpr <$> lexeme (choice ps) <?> "character literal"
  where
    ps =
      [ C.string "#\\tab" *> return '\t',
        C.string "#\\newline" *> return '\n',
        C.string "#\\return" *> return '\r',
        C.string "#\\space" *> return ' ',
        C.string "#\\\\" *> return '\\',
        C.string "#\\" *> L.charLiteral
      ]

stringLiteral :: Parser Expr
stringLiteral =
  StringExpr . T.pack
    <$> lexeme (C.char '\"' *> manyTill L.charLiteral (C.char '\"'))
    <?> "string literal"

fixnum :: Parser Expr
fixnum = FixnumExpr <$> (L.signed space integer <?> "fixnum")
  where
    integer = lexeme L.decimal

nil :: Parser Expr
nil = NilExpr <$ lexeme (C.string "()")

bool :: Parser Expr
bool = BoolExpr <$> (True <$ lexeme (C.string "#t") <|> False <$ lexeme (C.string "#f"))

symbol :: Parser Expr
symbol =
  SymbolExpr . T.pack
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
    [ bool,
      charLiteral,
      stringLiteral,
      try fixnum <|> symbol
    ]

identChars :: [Char]
identChars = "+-<>?=.*"

lambda :: Parser Expr
lambda = do
  _ <- word "("
  _ <- lexeme (C.string "lambda" <|> C.string "λ")
  args <- parens $ many symbol
  body <- expr
  _ <- word ")"
  return $ LambdaExpr args body

app :: Parser Expr
app = parens $ AppExpr <$> expr <*> many expr

expr :: Parser Expr
expr =
  choice
    [ immediate,
      try lambda <|> try app <|> nil
    ]
