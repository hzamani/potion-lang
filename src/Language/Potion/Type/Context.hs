module Language.Potion.Type.Context where

import Prelude hiding (lookup)

import Data.Foldable hiding (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid

import Language.Potion.Syntax
import Language.Potion.Type

newtype Context
  = Context (Map Name Scheme)
  deriving (Eq, Show)

empty :: Context
empty = Context Map.empty

base :: Context
base = fromList
  [ ("+", bin)
  , ("-", bin)
  , ("*", bin)
  , ("/", bin)
  , ("%", bin)
  , ("::", bin)
  , ("succ", Forall [] (tFun int int))
  ]
  where
    av = TVar "a"
    a = TV av
    bin = Forall [av] (tFun (tTuple [a, a]) a)
    int = TN "Int"

extend :: Context -> (Name, Scheme) -> Context
extend (Context schemes) (name, scheme)
  = Context $ Map.insert name scheme schemes

extends :: Context -> [(Name, Scheme)] -> Context
extends (Context schemes) vars
  = Context $ Map.union (Map.fromList vars) schemes

remove :: Context -> Name -> Context
remove (Context schemes) name
  = Context $ Map.delete name schemes

lookup :: Context -> Name -> Maybe Scheme
lookup (Context schemes) name
  = Map.lookup name schemes

merge :: Context -> Context -> Context
merge (Context a) (Context b)
  = Context $ Map.union a b

merges :: [Context] -> Context
merges = foldl' merge empty

singleton :: Name -> Scheme -> Context
singleton name scheme
  = Context $ Map.singleton name scheme

names :: Context -> [Name]
names (Context schemes)
  = Map.keys schemes

fromList :: [(Name, Scheme)] -> Context
fromList = Context . Map.fromList

toList :: Context -> [(Name, Scheme)]
toList (Context schemes) = Map.toList schemes

instance Monoid Context where
  mempty = empty
  mappend = merge
