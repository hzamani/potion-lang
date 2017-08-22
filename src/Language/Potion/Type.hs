module Language.Potion.Type where

import Data.List

import Language.Potion.Syntax

newtype TVar
  = TV Name
  deriving (Eq, Ord)

instance Show TVar where
  show (TV name) = name

data Type
  = TVar TVar
  | TCon Name
  | TList Type
  | TMap Type Type
  | TTuple [Type]
  | TFunc Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar var) = show var
  show (TCon name) = name
  show (TList a) = "[" ++ show a ++ "]"
  show (TMap a b) = "{" ++ show a ++ ": " ++ show b ++ "}"
  show (TTuple [t]) = show t
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show (TFunc a b) = show a ++ " -> " ++ show b

data Scheme
  = Forall [TVar] Type
  deriving Eq

instance Show Scheme where
  show (Forall [] ty) = show ty
  show (Forall vars ty) = "Ɐ" ++ intercalate "," (map show vars) ++ ". " ++ show ty

litteralType :: Literal -> Type
litteralType (LB _) = TCon "Bool"
litteralType (LI _) = TCon "Int"
litteralType (LF _) = TCon "Float"
litteralType (LC _) = TCon "Char"
litteralType (LS _) = TCon "String"
