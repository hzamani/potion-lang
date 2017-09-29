module Language.Potion.Parser (parseExpression, parseFile) where

import Data.Functor.Identity

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Exp
import qualified Text.Parsec.Token as Token

import Language.Potion.Type
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
    , "def", "end"
    , "match", "with"
    , "if", "then", "else"
    ]

reservedOpNames
  = [ ":"
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
  = [ [ bin "." ]
    , [ pre "-", pre "+" ]
    , [ bin "*", bin "/", bin "%" ]
    , [ bin "+", bin "-" ]
    , [ bin ">", bin ">=", bin "==", bin "<=", bin "<", bin "!=" ]
    , [ bin "?" ]
    , [ bin "=" ]
    , [ bin "@" ]
    , [ bin "::" ]
    ]
  where
    bin op = binary op (\x y -> EApp (en op) [x, y]) Exp.AssocLeft
    pre op = prefix op (\x   -> EApp (en $ op ++ "/1") [x])

    prefix :: String -> (a -> a) -> Exp.Operator String () Identity a
    prefix op f = Exp.Prefix (reservedOp op >> return f)

    binary :: String -> (a -> a -> a) -> Exp.Assoc -> Exp.Operator String () Identity a
    binary op f = Exp.Infix (reservedOp op >> return f)

operand :: Bool -> Parser Expression
operand False
  =   compositeExpression
  <|> EL <$> literal
  <|> EN <$> identifier
  <|> fun
  <|> match
  <|> ifThen

operand True
  =   operand False
  <|> const EPlace <$> reserved "_"

primaryExpression :: Bool -> Parser Expression
primaryExpression inPattern
  = do
    base <- operand inPattern
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
        return $ EApp (en "[..]") [base, index]

elements :: Expression -> Parser Expression
elements base
  = elements' <?> "elements"
  where
    elements'
      = do
        elems <- braces (commaSep element)
        return $ EApp (en "{}") (base:elems)

    element :: Parser Expression
    element
      = do
        key <- expression
        option key (keyValue key)

    keyValue key
      = do
        symbol ":"
        val <- expression
        return $ EApp (en "()") [key, val]

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
  <|> elements (en "")
  where
    list = EApp (en "[]") <$> brackets expressionList
    tuple = do
      list <- parens expressionList
      return $ case list of
        [x] -> x
        xs  -> EApp (en "()") xs

expressionList :: Parser [Expression]
expressionList
  = commaSep expression <?> "expression list"

identifier :: Parser Name
identifier
  = UN <$> Token.identifier lexer <?> "identifier"

identifierOrPlace :: Parser Expression
identifierOrPlace
  = EN <$> identifier <|> place
  where
    place = const EPlace <$> symbol "_" <?> "placeholder"

fun :: Parser Expression
fun
  = do
    symbol "#"
    choice [shorFun, simpleFun]
  where
    simpleFun = do
      params <- parens (commaSep identifierOrPlace) <?> "parameters"
      fatArrow
      body <- expression
      return $ EFun params body

    shorFun = do
      body <- between pipe pipe patter <?> "shortbody"
      return $ EFun [EPlace] body

    pipe = symbol "|"

block :: Parser Expression
block
  = do
    exprs <- manyTill expression end
    return $ EApp (en "%block%") exprs

end = reserved "end"

fatArrow = reservedOp "=>"

match :: Parser Expression
match
  = do
      reserved "match"
      expr <- expression
      branches <- manyTill matchCase end
      return $ EMatch expr branches

matchCase :: Parser (Expression, Expression, Expression)
matchCase
  = do
      reserved "with"
      pat <- patter
      pred <- option EPlace when
      fatArrow
      expr <- expression
      return (pat, pred, expr)
  where
    when
      = do
          reserved "when"
          expression

ifThen :: Parser Expression
ifThen
  = do
      reserved "if"
      predicate <- expression
      reserved "then"
      onTrue <- expression
      reserved "else"
      onFalse <- expression
      return $ EApp (en "if") [predicate, onTrue, onFalse]

expression :: Parser Expression
expression
  = Exp.buildExpressionParser table (primaryExpression False) <?> "expression"

patter :: Parser Expression
patter
  = Exp.buildExpressionParser table (primaryExpression True) <?> "pattern"

contents :: Parser a -> Parser a
contents p
  = do
    whiteSpace
    result <- p
    eof
    return result

declaration :: Parser Declaration
declaration
  =   def
  <|> defForeign

def :: Parser Declaration
def
  = do
    reserved "def"
    name <- identifier
    params <- parens (commaSep identifierOrPlace) <?> "parameters"
    body <- choice [shortDef, block]
    return $ DDef name params body
  where
    shortDef
      = do
        reservedOp "="
        expression

defForeign :: Parser Declaration
defForeign
  = do
    reserved "foreign"
    path <- Token.stringLiteral lexer
    name <- identifier
    reservedOp ":"
    ins <- expression
    reservedOp "->"
    outs <- expression
    return $ DForeign path name $ tFun (toType ins) (toType outs)

parseExpression :: String -> Either ParseError Expression
parseExpression
  = parse (contents expression) "<stdin>"

parseFile :: String -> String -> Either ParseError [Declaration]
parseFile
  = parse $ contents $ many declaration
