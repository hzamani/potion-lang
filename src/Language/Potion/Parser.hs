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
potionDef
  = Token.LanguageDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.nestedComments  = False
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> oneOf "_"
    , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.reservedNames   = reservedNames
    , Token.reservedOpNames = reservedOpNames
    , Token.caseSensitive   = True
    }

reservedNames
  = [ "true", "false"
    , "def", "do", "end"
    , "match", "with"
    ]

reservedOpNames
  = [ "::"
    , "#" , "=>"
    , "="
    , ">", ">=", "==", "<=", "<", "!="
    , "@"
    , "."
    , "?"
    , "-", "+"
    , "*", "/", "%"
    , "+", "-"
    ]

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

braces :: Parser a -> Parser a
braces = Token.braces lexer

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
table
  = [ [ bin "::" ]
    , [ bin "=" ]
    , [ bin ">", bin ">=", bin "==", bin "<=", bin "<", bin "!=" ]
    , [ bin "@" ]
    , [ bin "." ]
    , [ bin "?" ]
    , [ pre "-", pre "+" ]
    , [ bin "*", bin "/", bin "%" ]
    , [ bin "+", bin "-" ]
    ]
  where
    bin op = binary op (\x y -> EApp (EN op) [x, y]) Exp.AssocLeft
    pre op = prefix op (\x   -> EApp (EN $ op ++ "/1") [x])

    prefix :: String -> (a -> a) -> Exp.Operator String () Identity a
    prefix op f = Exp.Prefix (reservedOp op >> return f)

    binary :: String -> (a -> a -> a) -> Exp.Assoc -> Exp.Operator String () Identity a
    binary op f = Exp.Infix (reservedOp op >> return f)

operand :: Parser Expression
operand
  =   compositeExpression
  <|> EL <$> literal
  <|> EN <$> identifier
  <|> fun

primaryExpression :: Parser Expression
primaryExpression
  = do
    base <- operand
    primary base
  where
    primary base = option base (part base)
    part base =
      do
        base' <- arguments base <|> slice base <|> elements base
        primary base'

arguments :: Expression -> Parser Expression
arguments base
  = arguments' <?> "arguments"
  where
    arguments' =
      do
        args <- parens expressionList
        return $ EApp base args

slice :: Expression -> Parser Expression
slice base
  = slice' <?> "slice"
  where
    slice' =
      do
        index <- brackets expression
        return $ EApp (EN "[..]") [base, index]

elements :: Expression -> Parser Expression
elements base
  = elements' <?> "elements"
  where
    elements'
      = do
        elems <- braces (commaSep element)
        return $ EApp (EN "{}") (base:elems)

    element :: Parser Expression
    element
      = do
        key <- expression
        option key (keyValue key)

    keyValue key
      = do
        symbol ":"
        val <- expression
        return $ EApp (EN "()") [key, val]

literal :: Parser Literal
literal
  =   bool
  <|> number
  <|> charLit
  <|> strLit
  <?> "literal"
  where
    charLit = LC <$> Token.charLiteral lexer
    strLit = LS <$> Token.stringLiteral lexer
    number = either LI LF <$> Token.naturalOrFloat lexer
    true = const (LB True) <$> reserved "true"
    false = const (LB False) <$> reserved "false"
    bool = true <|> false

compositeExpression :: Parser Expression
compositeExpression
  =   list
  <|> tuple
  <|> elements (EN "")
  where
    list = EApp (EN "[]") <$> brackets expressionList
    tuple = do
      list <- parens expressionList
      return $ case list of
        [x] -> x
        xs  -> EApp (EN "()") xs

expressionList :: Parser [Expression]
expressionList
  = commaSep expression <?> "expression list"

identifier :: Parser Name
identifier
  = Token.identifier lexer <?> "identifier"

fun :: Parser Expression
fun
  = do
    reservedOp "#"
    args <- parens (commaSep identifier) <?> "parameters"
    reservedOp "=>"
    body <- expression
    return $ EFun args body

expression :: Parser Expression
expression
  = Exp.buildExpressionParser table primaryExpression <?> "expression"

contents :: Parser a -> Parser a
contents p
  = do
    whiteSpace
    result <- p
    eof
    return result

parseExpression :: String -> Either ParseError Expression
parseExpression
  = parse (contents expression) "<stdin>"
