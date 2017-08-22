module Language.Potion.Syntax where

type Name = String

data Expression
  = Lit Literal
  | Var Name
  | Op Name
  | Func [Name] Expression
  | Tuple [Expression]
  | App Expression [Expression]
  deriving (Eq, Show)

data Literal
  = LB Bool
  | LI Integer
  | LF Double
  | LC Char
  | LS String
  deriving (Eq, Show)

-- type Declaration = (Name, Expression)

-- data Program
--   = Program [Declaration] Expression
--   deriving (Eq, Show)
