module Language.Potion.Type where

import Data.List

newtype TVar
  = TVar String
  deriving (Eq, Ord)

instance Show TVar where
  show (TVar name) = name

data Type
  = TN String
  | TV TVar
  | TApp Type [Type]
  deriving (Eq, Ord)

instance Show Type where
  show (TN name) = name
  show (TV var) = show var
  show (TApp (TN "Tuple") [t]) = show t
  show (TApp (TN "Tuple") ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show (TApp (TN "List") [a]) = "[" ++ show a ++ "]"
  show (TApp (TN "Fun") [a, b]) = show a ++ " -> " ++ show b
  show (TApp (TN "Map") [a, b]) = "{" ++ show a ++ ": " ++ show b ++ "}"
  show (TApp con args@(_:_)) = show con ++ show args
  show (TApp con args) = show con ++ "(" ++ intercalate ", " (map show args) ++ ")"

data Scheme
  = Forall [TVar] Type
  deriving Eq

instance Show Scheme where
  show (Forall [] ty) = show ty
  show (Forall vars ty) = "Ɐ" ++ intercalate "," (map show vars) ++ ". " ++ show ty

tList a  = TApp (TN "List") [a]
tFun a b = TApp (TN "Fun") [a, b]
tMap a b = TApp (TN "Map") [a, b]
tTuple   = TApp (TN "Tuple")
