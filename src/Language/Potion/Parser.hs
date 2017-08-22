module Language.Potion.Parser (parseExpression) where

import Data.Functor.Identity

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Exp
import qualified Text.Parsec.Token as Token

import Language.Potion.Syntax

potionDef :: Token.LanguageDef ()
potionDef = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.nestedComments  = False
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_?"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = ["true", "false"]
  , Token.reservedOpNames = [".", "+", "-", "*", "/", "%", "#", "=>"]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser potionDef

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

table :: Exp.OperatorTable String () Identity Expression
table =
  [ [ bin "." ]
  , [ pre "-" ]
  , [ bin "*", bin "/", bin "%" ]
  , [ bin "+", bin "-" ]
  ]
  where
    bin op = binary op (\x y -> App (Op op) [x, y]) Exp.AssocLeft
    pre op = prefix op (\x   -> App (Op op) [x])

    prefix :: String -> (a -> a) -> Exp.Operator String () Identity a
    prefix op f = Exp.Prefix (reservedOp op >> return f)

    binary :: String -> (a -> a -> a) -> Exp.Assoc -> Exp.Operator String () Identity a
    binary op f = Exp.Infix (reservedOp op >> return f)

operand :: Parser Expression
operand =
  parens expression
  <|> Lit <$> literal
  <|> funcLiteral
  <|> Var <$> identifier

primaryExpression :: Parser Expression
primaryExpression =
  do
    base <- operand
    composit' base
  where
    composit' base = option base (composit'' base)
    composit'' base =
      do
        f <- arguments <|> slice
        composit' (f base)

arguments :: Parser (Expression -> Expression)
arguments = arguments' <?> "argument list"
  where
    arguments' =
      do
        args <- parens $ commaSep expression
        return $ \base -> App base args

slice :: Parser (Expression -> Expression)
slice = slice' <?> "slice"
  where
    slice' =
      do
        index <- brackets expression
        return $ \base -> App (Op "[]") [index]

literal :: Parser Literal
literal = bool <|> number <|> charLit <|> strLit <?> "literal"
  where
    charLit = LC <$> Token.charLiteral lexer
    strLit = LS <$> Token.stringLiteral lexer
    number = either LI LF <$> Token.naturalOrFloat lexer
    true = const (LB True) <$> reserved "true"
    false = const (LB False) <$> reserved "false"
    bool = true <|> false

identifier :: Parser Name
identifier = Token.identifier lexer <?> "identifier"

funcLiteral :: Parser Expression
funcLiteral =
  do
    reservedOp "#"
    args <- parens (commaSep identifier) <?> "parameters"
    reservedOp "=>"
    body <- expression
    return $ Func args body

expression :: Parser Expression
expression = Exp.buildExpressionParser table primaryExpression <?> "expression"

contents :: Parser a -> Parser a
contents p =
  do
    whiteSpace
    result <- p
    eof
    return result

parseExpression :: String -> Either ParseError Expression
parseExpression = parse (contents expression) "<stdin>"
