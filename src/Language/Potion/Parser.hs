module Language.Potion.Parser
  ( parseExpression
  , parse
  ) where

import Data.Functor.Identity

import Control.Monad
import Data.Maybe (catMaybes)
import Text.Parsec hiding (parse)
import Text.Parsec.Language (emptyDef)

import qualified Data.Map.Strict as Map
import qualified Text.Parsec.Expr as Exp
import qualified Text.Parsec.Token as Token

import Language.Potion.Type
import Language.Potion.Syntax

data OperatorSpec
  = Infix Char Int
  | Postfix Int
  | Prefix Int
  deriving (Eq, Show)

data PState
  = PState
    { parsers :: [Parser Expression]
    , operators :: [OperatorSpec]
    }

type Parser = ParsecT String PState Identity

emptyPState = PState [] []

potionLang :: Token.LanguageDef PState
potionLang
  = Token.LanguageDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.nestedComments  = False
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> oneOf "_?!"
    , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.reservedNames   = reservedNames
    , Token.reservedOpNames = reservedOpNames
    , Token.caseSensitive   = True
    }

reservedNames
  = [ "true", "false"
    , "def" , "end"
    , "match", "with", "when"
    -- , "if", "then", "else"
    ]

reservedOpNames
  = [ ":"
    , "#" , "=>"
    , "="
    , ">", ">=", "==", "<=", "<", "!="
    , "."
    , ".."
    , "..."
    , "-", "+"
    , "*", "/", "%"
    ]

lexer :: Token.TokenParser PState
lexer = Token.makeTokenParser potionLang

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

table :: Exp.OperatorTable String PState Identity Expression
table
  = [ [ rbin "." ]
    , [ pos "..."]
    , [ pre "-", pre "+" ]
    , [ bin "*", bin "/", bin "%" ]
    , [ bin "+", bin "-" ]
    , [ bin ">", bin ">=", bin "==", bin "<=", bin "<", bin "!=" ]
    , [ bin "=" ]
    ]
  where
    rbin op = binary  op (\x y -> eOp op [x, y]) Exp.AssocRight
    bin op  = binary  op (\x y -> eOp op [x, y]) Exp.AssocLeft
    pre op  = prefix  op (\x   -> eOp op [x])
    pos op  = postfix op (\x   -> eOp op [x])

    prefix :: String -> (a -> a) -> Exp.Operator String PState Identity a
    prefix op f = Exp.Prefix (reservedOp op >> return f)

    postfix :: String -> (a -> a) -> Exp.Operator String PState Identity a
    postfix op f = Exp.Postfix (reservedOp op >> return f)

    binary :: String -> (a -> a -> a) -> Exp.Assoc -> Exp.Operator String PState Identity a
    binary op f = Exp.Infix (reservedOp op >> return f)

withPos :: Parser Expression -> Parser Expression
withPos p
  = do
    pos <- getPosition
    exp <- p
    return $ putPos (Pos pos) exp

literal :: Parser Literal
literal
  =   bool
  <|> int
  <|> charLit
  <|> strLit
  <?> "literal"
  where
    charLit = LC <$> Token.charLiteral lexer
    strLit  = LS <$> Token.stringLiteral lexer
    int     = LI <$> Token.natural lexer
    true    = const (LB True) <$> reserved "true"
    false   = const (LB False) <$> reserved "false"
    bool    = true <|> false

literalExp :: Parser Expression
literalExp = withPos $ eLit <$> literal

identifier :: Parser Name
identifier = Token.identifier lexer <?> "identifier"

nameExp :: Parser Expression
nameExp = withPos $ eName <$> identifier

holeExp :: Parser Expression
holeExp = withPos (symbol "_" >> return eHole <?> "hole")

param :: Parser Expression
param = nameExp <|> holeExp <?> "parameter"

fatArrow = reservedOp "=>"
pipe     = symbol "|"

funExp :: Parser Expression
funExp = withPos (symbol "#" >> choice [shorFun, fun] <?> "function")
  where
    fun = do
      params <- parens (commaSep param) <?> "parameters"
      fatArrow
      body <- expression
      return $ eFun params body

    shorFun = do
      body <- between pipe pipe expression <?> "shortbody"
      return $ eFun [eHole] body

listExp :: Parser Expression
listExp = withPos $ eOp "[]" <$> list
  where
    list = brackets (commaSep expression) <?> "list"

tupleExp :: Parser Expression
tupleExp = withPos tuple <?> "tuple"
  where
    tuple = do
      xs <- parens (commaSep expression)
      case xs of
        [x] -> return x
        xs' -> return $ eOp "()" xs'

pair :: Parser Expression
pair = withPos keyvalue <?> "pair"
  where
    keyvalue = do
      key <- expression
      option key (value key)

    value key = do
      symbol ":"
      val <- expression
      return $ eOp "(:)" [key, val]

elements :: Expression -> Parser Expression
elements on = do
  elems <- braces (commaSep pair) <?> "elements"
  return $ eOp "{}" (on:elems)

mapExp :: Parser Expression
mapExp = withPos (elements eHole) <?> "map"

blockTill :: Parser () -> Parser Expression
blockTill end
  = withPos blk <?> "block"
  where
    blk = do
      exps <- manyTill expression end
      case exps of
        [exp] -> return exp
        _     -> return $ eOp "%block%" exps

block :: Parser Expression
block
  = withPos blk <?> "block"
  where
    blk = do
      exps <- many1 expression
      case exps of
        [exp] -> return exp
        _     -> return $ eOp "%block%" exps

matchExp :: Parser Expression
matchExp = withPos match <?> "match"
  where
    match = do
      reserved "match"
      exp <- expression
      cases <- manyTill caseExp (reserved "end")
      return $ eMatch exp cases

caseExp :: Parser Expression
caseExp
  = do
      (pat, pred) <- with <|> when eHole
      fatArrow
      exp <- block
      return $ eCase pat pred exp
  where
    with = do
      reserved "with"
      pat <- expression
      option (pat, eHole) (when pat)

    when pat = do
      reserved "when"
      pred <- expression
      return (pat, pred)

term :: Parser Expression
term
  =   try literalExp
  <|> nameExp
  <|> funExp
  <|> holeExp
  <|> listExp
  <|> tupleExp
  <|> mapExp
  <|> matchExp
  <?> "term"

arguments :: Expression -> Parser Expression
arguments f
  = withPos args <?> "arguments"
  where
    args = do
      args <- parens (commaSep expression)
      return $ eApp f args

slice :: Expression -> Parser Expression
slice arr
  = withPos (brackets $ try slc <|> idx) <?> "slice"
  where
    idx = do
      index <- expression
      return $ eOp "[..]" [arr, index]

    slc = do
      start <- option eHole expression
      reservedOp ".."
      end <- option eHole expression
      return $ eOp "[..]" [arr, start, end]

operand :: Parser Expression
operand = term >>= composite <?> "composite-expression"
  where
    composite base = option base $ part base
    part base = do
      newBase <- arguments base <|> slice base <|> elements base
      composite newBase

expression :: Parser Expression
expression
  = Exp.buildExpressionParser table operand <?> "expression"

funDef :: Pos -> Parser Definition
funDef pos
  = do
    params <- parens (commaSep param) <?> "parameters"
    body <- choice [shortBody, longBody] <?> "body"
    return $ DFun pos $ EFun pos params body
  where
    shortBody = reservedOp "=" >> expression
    longBody = do
      exps <- block
      reserved "end"
      return exps

definition :: Parser (Name, Definition)
definition
  = do
    pos <- Pos <$> getPosition
    reserved "define"
    name <- identifier
    def <- funDef pos -- <|> sigDef <|> foreignDef
    return (name, def)

code :: Name -> Parser Code
code name
  = do
    defs <- contents $ many definition
    return $ Code name $ Map.fromList defs

contents :: Parser a -> Parser a
contents p
  = do
    whiteSpace
    result <- p
    eof
    return result

parseExpression :: String -> Either ParseError Expression
parseExpression
  = runParser (contents expression) emptyPState "<stdin>"

parse :: Name -> String -> Either ParseError Code
parse name
  = runParser (code name) emptyPState name

