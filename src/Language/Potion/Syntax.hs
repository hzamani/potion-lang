module Language.Potion.Syntax where

import Debug.Trace

import Language.Potion.Type

debug :: Show a => String -> a -> a
debug msg x = trace (msg ++ ": " ++ show x) x

type UserName = String

data Name
  = UN UserName          -- user name
  | PN UserName [String] -- parametric name
  deriving (Eq, Ord, Show)

data Expression
  = EApp Expression [Expression]
  | EFun [Expression] Expression
  | EMatch Expression [(Expression, Expression, Expression)] -- (pattern, when, expr)
  | EL Literal
  | EN Name
  | EPlace
  | ET Expression Type
  deriving (Eq, Show)

en :: String -> Expression
en = EN . UN

un :: Name -> UserName
un (UN name)   = name
un (PN name _) = name

eArray = EApp (en "[]")
eMap   = EApp (en "{}")
eFun   = EApp (en "#")
eTuple = EApp (en "()")
eList  = EApp (en "#[]")
eHash  = EApp (en "#{}")

eArrayT ty = EApp (ET (en "[]") ty)
eMapT ty   = EApp (ET (en "{}") ty)
eFunT ty   = EApp (ET (en "#") ty)
eTupleT ty = EApp (ET (en "()") ty)
eListT ty  = EApp (ET (en "#[]") ty)
eHashT ty  = EApp (ET (en "#{}") ty)

typeof :: Expression -> Type
typeof (ET _ ty) = ty
typeof _ = TUnknown

walk :: (Expression -> Expression) -> Expression -> Expression
walk f (EApp exp args) = f $ EApp (walk f exp) (map (walk f) args)
walk f (EMatch exp cases) = f $ EMatch (walk f exp) (map (walk3 f) cases)
walk f (EFun params body) = f $ EFun (map (walk f) params) (walk f body)
walk f (ET exp ty) = f $ ET (walk f exp) ty
walk f exp = f exp

walk3 f (a, b, c) = (walk f a, walk f b, walk f c)

replace :: Expression -> Expression -> Expression -> Expression
replace x y
  = walk rep
  where
    rep exp = if exp == x then y else exp

data Declaration
  = DDef Name [Expression] Expression
  | DSig Name Type
  -- | DData Name Constructors
  -- | DIFace Name Constraint [Declaration]
  -- | DImpl Name Type Constraint [Declaration] -- interface type
  deriving (Eq, Show)

type Definition = (Name, Expression)

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
