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
    , "match", "with", "when"
    , "if", "then", "else"
    ]

reservedOpNames
  = [ ":"
    , "#" , "=>"
    , "="
    , ">", ">=", "==", "<=", "<", "!="
    , "@"
    , "."
    , "..."
    , "_"
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
    , [ pos "..."]
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
    bin op = binary  op (\x y -> EApp (en op) [x, y]) Exp.AssocLeft
    pre op = prefix  op (\x   -> EApp (en $ op ++ "/1") [x])
    pos op = postfix op (\x   -> EApp (en op) [x])

    prefix :: String -> (a -> a) -> Exp.Operator String () Identity a
    prefix op f = Exp.Prefix (reservedOp op >> return f)

    postfix :: String -> (a -> a) -> Exp.Operator String () Identity a
    postfix op f = Exp.Postfix (reservedOp op >> return f)

    binary :: String -> (a -> a -> a) -> Exp.Assoc -> Exp.Operator String () Identity a
    binary op f = Exp.Infix (reservedOp op >> return f)

operand :: Bool -> Parser Expression
operand False
  =   compositeExpression False
  <|> fun
  <|> match
  <|> ifThen
  <|> EL <$> literal
  <|> EN <$> identifier

operand True
  =   const ENothing <$> symbol "_"
  <|> const (eSpread [ENothing]) <$> symbol "..."
  <|> compositeExpression True
  <|> operand False

primaryExpression :: Bool -> Parser Expression
primaryExpression inPattern
  = do
    base <- operand inPattern
    primary base
  where
    primary base = option base (part base)
    part base =
      do
        base' <- arguments base <|> slice base <|> elements inPattern base
        primary base'

arguments :: Expression -> Parser Expression
arguments base
  = arguments' <?> "arguments"
  where
    arguments' =
      do
        args <- parens (expressionList False)
        return $ EApp base args

slice :: Expression -> Parser Expression
slice base
  = brackets slice' <?> "slice"
  where
    slice' =
      do
        index <- expression False
        args <- option [base, index] (sliceRange base index)
        return $ EApp (en "[:]") args

sliceRange base start
  = do
    reservedOp ":"
    end <- option ENothing $ expression False
    return [base, start, end]

elements :: Bool -> Expression -> Parser Expression
elements inPattern base
  = elements' <?> "elements"
  where
    elements'
      = do
        elems <- braces (commaSep element)
        return $ EApp (en "{}") (base:elems)

    element :: Parser Expression
    element
      = do
        key <- expression inPattern
        option key (keyValue key)

    keyValue key
      = do
        symbol ":"
        val <- expression inPattern
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

compositeExpression :: Bool -> Parser Expression
compositeExpression inPattern
  =   list
  <|> tuple
  <|> elements inPattern (en "")
  where
    list = EApp (en "[]") <$> brackets (expressionList inPattern)
    tuple = do
      list <- parens (expressionList inPattern)
      return $ case list of
        [x] -> x
        xs  -> EApp (en "()") xs

expressionList :: Bool -> Parser [Expression]
expressionList inPattern
  = commaSep (expression inPattern) <?> "expression list"

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
      body <- expression False
      return $ EFun params body

    shorFun = do
      body <- between pipe pipe (expression True) <?> "shortbody"
      return $ EFun [EPlace] body

    pipe = symbol "|"

inlineBlock :: Parser Expression
inlineBlock
  = do
    exps <- many1 (expression False)
    return $ EApp (en "%block%") exps

block :: Parser Expression
block
  = do
    exps <- manyTill (expression False) end
    return $ EApp (en "%block%") exps

end = reserved "end"

fatArrow = reservedOp "=>"

match :: Parser Expression
match
  = do
      reserved "match"
      expr <- expression False
      branches <- manyTill matchCase end
      return $ EMatch expr branches

matchCase :: Parser (Expression, Expression, Expression)
matchCase
  = do
      (pat, pred) <- with <|> when ENothing
      fatArrow
      exps <- inlineBlock
      return (pat, pred, exps)
  where
    with = do
      reserved "with"
      pat <- expression True
      option (pat, ENothing) (when pat)

    when pat = do
      reserved "when"
      pred <- expression False
      return (pat, pred)

ifThen :: Parser Expression
ifThen
  = do
    reserved "if"
    predicate <- expression False
    reserved "then"
    onTrue <- expression False
    reserved "else"
    onFalse <- expression False
    return $ EApp (en "if") [predicate, onTrue, onFalse]

expression :: Bool -> Parser Expression
expression False
  = Exp.buildExpressionParser table (primaryExpression False) <?> "expression"
expression True
  = Exp.buildExpressionParser table (primaryExpression True) <?> "pattern"

-- patter :: Parser Expression
-- patter
--   = Exp.buildExpressionParser table (primaryExpression True) <?> "pattern"

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
  <|> sig
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
        expression False

sig :: Parser Declaration
sig
  = do
    reserved "signature"
    (name, ty) <- signature
    return $ DSig name ty

signature :: Parser (Name, Type)
signature
  = do
    name <- identifier
    reservedOp ":"
    ins <- expression False
    reservedOp "->"
    outs <- expression False
    return (name, toFun ins outs)

defForeign :: Parser Declaration
defForeign
  = do
    reserved "foreign"
    path <- Token.stringLiteral lexer
    (name, ty) <- signature
    return $ DForeign path name ty

parseExpression :: String -> Either ParseError Expression
parseExpression
  = parse (contents $ expression False) "<stdin>"

parseFile :: String -> String -> Either ParseError SourceFile
parseFile
  = parse $ contents $ File <$> many declaration
