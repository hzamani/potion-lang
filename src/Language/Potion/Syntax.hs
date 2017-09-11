module Language.Potion.Syntax where

import Language.Potion.Type

type Name = String

data Expression
  = EApp Expression [Expression]
  | EFun [Expression] Expression
  | EMatch Expression [(Expression, Expression, Expression)] -- (pattern, when, expr)
  | EL Literal
  | EN Name
  | EPlace
  deriving (Eq, Show)

-- applyExpr f (EApp expr args) = EApp (applyExpr f expr) (map (applyExpr f) args)
-- applyExpr f (EMatch expr cases) = EMatch (applyExpr f expr) (map (\(x,y,z) -> (applyExpr f x,applyExpr f y,applyExpr f z)) cases)
-- applyExpr f (EFun params body) = EFun (map (applyExpr f) params) (applyExpr f body)
-- applyExpr f expr = f expr

-- replace x y
--   = applyExpr rep
--   where
--     rep expr = if expr == x then y else expr

data Declaration
  = DDef Name [Expression] Expression
  | DSig Name Type
  -- | DData Name Constructors
  -- | DIFace Name Constraint [Declaration]
  -- | DImpl Name Type Constraint [Declaration] -- interface type
  deriving (Eq, Show)

newtype SourceFile
  = File [Declaration]
  deriving (Eq, Show)

data Literal
  = LB Bool
  | LI Integer
  | LF Double
  | LC Char
  | LS String
  deriving (Eq, Show)

litteralType :: Literal -> Type
litteralType (LB _) = TN "Bool"
litteralType (LI _) = TN "Int"
litteralType (LF _) = TN "Float"
litteralType (LC _) = TN "Char"
litteralType (LS _) = TN "String"


-- data Program
--   = Program [Import] [(Access, Declaration)]
--   deriving (Eq, Show)

-- data Access
--   = Private
--   | Public
--   deriving (Eq, Show)

-- data Import
--   = Import Name Name -- import repo.module
--   deriving (Eq, Show)
