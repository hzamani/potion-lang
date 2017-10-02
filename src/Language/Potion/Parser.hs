module Language.Potion.Parser (parseExpression, parseFile) where

import Data.Functor.Identity

import Control.Monad
import Data.Maybe (catMaybes)
import Text.Parsec
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

withMeta :: Parser Expression -> Parser Expression
withMeta p
  = do
    pos <- getPosition
    exp <- p
    return $ putMeta (metaFromPos pos) exp

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
literalExp = withMeta $ eLit <$> literal

identifier :: Parser Name
identifier = Token.identifier lexer <?> "identifier"

nameExp :: Parser Expression
nameExp = withMeta $ eName <$> identifier

holeExp :: Parser Expression
holeExp = withMeta (symbol "_" >> return eHole <?> "hole")

param :: Parser Expression
param = nameExp <|> holeExp <?> "parameter"

fatArrow = reservedOp "=>"
pipe     = symbol "|"

funExp :: Parser Expression
funExp = withMeta (symbol "#" >> choice [shorFun, fun] <?> "function")
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
listExp = withMeta $ eOp "[]" <$> list
  where
    list = brackets (commaSep expression) <?> "list"

tupleExp :: Parser Expression
tupleExp = withMeta tuple <?> "tuple"
  where
    tuple = do
      xs <- parens (commaSep expression)
      case xs of
        [x] -> return x
        xs' -> return $ eOp "()" xs'

pair :: Parser Expression
pair = withMeta keyvalue <?> "pair"
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
mapExp = withMeta (elements eHole) <?> "map"

blockTill :: Parser () -> Parser Expression
blockTill end
  = withMeta blk <?> "block"
  where
    blk = do
      exps <- manyTill expression end
      case exps of
        [exp] -> return exp
        _     -> return $ eOp "%block%" exps

block :: Parser Expression
block
  = withMeta blk <?> "block"
  where
    blk = do
      exps <- many1 expression
      case exps of
        [exp] -> return exp
        _     -> return $ eOp "%block%" exps

matchExp :: Parser Expression
matchExp = withMeta match <?> "match"
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

ifExp :: Parser Expression
ifExp = withMeta ifThen <?> "if"
  where
    ifThen = do
      reserved "if"
      predicate <- expression
      onTrue <- block
      reserved "else"
      onFalse <- block
      optional $ reserved "end"
      return $ eOp "if" [predicate, onTrue, onFalse]

syntax :: Parser (Parser Expression)
syntax
  = do
    nameP <- sym
    argsP <- manyTill syntaxTerm (reserved "end")
    return $ builder nameP argsP
  where
    syntaxTerm :: Parser (Parser (Either String Expression))
    syntaxTerm = exp <|> sym

    builder :: Parser (Either String Expression) -> [Parser (Either String Expression)] -> Parser Expression
    builder nameP partsP = do
      Left name <- nameP
      args <- concatMap (either (const []) return) <$> sequence partsP
      return $ eOp name args

    ss = lexeme $ try $ char ':' >> many1 letter

    sym :: Parser (Parser (Either String Expression))
    sym = do
      s <- ss
      return (Left <$> symbol s <?> s)

    exp :: Parser (Parser (Either String Expression))
    exp = do
      symbol "%"
      ty <- choice (map symbol ["exp", "block"])
      case ty of
        "exp"   -> return (Right <$> expression)
        "block" -> do
          end <- ss
          return (Right <$> blockTill (symbol end >> return ()))
        -- "end"   -> return (const (Left "end") <$> reserved "end")
    -- syntaxParser :: Parser String -> Parser [Maybe Expression] -> Parser Expression
    -- syntaxParser nameP argsP = do
    --   name <- nameP
    --   args <- argsP
    --   return $ eOp name (catMaybes args)

    -- sterms :: Parser [Maybe Expression] -> Parser (Parser [Maybe Expression])
    -- -- sterms p = return $ sjoin p $ option (return []) (sterms $ return sterm)
    -- sterms p = return p

    -- sjoin :: Parser [Maybe Expression] -> Parser [Maybe Expression] -> Parser [Maybe Expression]
    -- sjoin p1 p2 = do
    --   xs1 <- p1
    --   xs2 <- p2
    --   return $ xs1 ++ xs2

    -- sterm :: Parser (Parser [Maybe Expression])
    -- sterm = exp <|> exps <|> symTerm

    -- symTerm :: Parser (Parser [Maybe Expression])
    -- symTerm = sym >> return [Nothing]


    -- -- opt :: Parser [Maybe Expression]
    -- -- opt = symbol "?" >> return $ optional sterm

    -- exp :: Parser (Parser [Maybe Expression])
    -- exp = symbol "%exp" >> (\x -> [Just x]) <$> expression

    -- exps :: Parser (Parser [Maybe Expression])
    -- exps = symbol "%exp..." >> map Just <$> many1 expression

syntaxExp :: Parser Expression
syntaxExp = do
  symbol "syntax"
  p <- syntax
  res <- p
  return res

term :: Parser Expression
term
  -- get state
  -- build parser
  =   try literalExp
  <|> nameExp
  <|> funExp
  <|> holeExp
  <|> listExp
  <|> tupleExp
  <|> mapExp
  <|> matchExp
  <|> ifExp
  <?> "term"

arguments :: Expression -> Parser Expression
arguments f
  = withMeta args <?> "arguments"
  where
    args = do
      args <- parens (commaSep expression)
      return $ eApp f args

slice :: Expression -> Parser Expression
slice arr
  = withMeta (brackets $ try slc <|> idx) <?> "slice"
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
  -- get state
  -- build infixtable
  = Exp.buildExpressionParser table operand <?> "expression"

funDef :: SourcePos -> Parser Definition
funDef pos
  = do
    params <- parens (commaSep param) <?> "parameters"
    body <- choice [shortBody, longBody] <?> "body"
    let meta = metaFromPos pos
    return $ DFun meta $ EFun meta params body
  where
    shortBody = reservedOp "=" >> expression
    longBody = do
      exps <- block
      reserved "end"
      return exps

definition :: Parser (Name, Definition)
definition
  = do
    pos <- getPosition
    reserved "def"
    name <- identifier
    def <- funDef pos -- <|> sigDef <|> foreignDef
    return (name, def)

file :: Name -> Parser SourceFile
file name
  = do
    code <- many definition
    return $ File name $ Map.fromList code

contents :: Parser a -> Parser a
contents p
  = do
    whiteSpace
    result <- p
    eof
    return result

parseExpression :: String -> Either ParseError Expression
parseExpression
  = runParser (contents syntaxExp) emptyPState "<stdin>"

parseFile :: Name -> String -> Either ParseError Expression
parseFile name
  = runParser (contents syntaxExp) emptyPState name

-- parseFile :: Name -> String -> Either ParseError SourceFile
-- parseFile name
--   = runParser (contents $ file name) emptyPState name
