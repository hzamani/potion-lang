module Language.Potion.Type.Context where

import Prelude hiding (lookup)

import Data.Foldable hiding (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax
import Language.Potion.Type

type Variants = Set Type
type Apps = Map Name (Set Type)
type Fun = (Scheme, Apps, Variants)

newtype Context
  = Context (Map Name Fun)
  deriving (Eq, Show)

insertApp :: Name -> Type -> Apps -> Apps
insertApp name ty
  = Map.insertWith Set.union name (Set.singleton ty)

mun :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Map k (Set a)
mun = Map.unionWith Set.union

muns :: (Ord k, Ord a) => [Map k (Set a)] -> Map k (Set a)
muns = Map.unionsWith Set.union

empty :: Context
empty = Context Map.empty

base :: Context
base = fromList
  [ ("+", bin) , ("-", bin) , ("*", bin) , ("/", bin) , ("%", bin)
  , ("::", bin) , ("@", bin)
  , (">", bool) , (">=", bool) , ("==", bool) , ("!=", bool) , ("<", bool) , ("<=", bool)
  ]
  where
    av = TVar "a"
    a = TV av
    aa = tTuple [a, a]
    int = TN "Int"
    bin = fun $ Forall [av] (tFun aa a)
    bool = fun $ Forall [av] (tFun aa (TN "Bool"))

fun :: Scheme -> Fun
fun scheme = (scheme, Map.empty, Set.empty)

extend :: Context -> (Name, Scheme) -> Context
extend (Context funs) (name, scheme)
  = Context $ Map.insert name (fun scheme) funs

extends :: Context -> [(Name, Scheme)] -> Context
extends (Context funs) vars
  = Context $ Map.union (Map.map fun $ Map.fromList vars) funs

remove :: Context -> Name -> Context
remove (Context funs) name
  = Context $ Map.delete name funs

lookup :: Context -> Name -> Maybe Fun
lookup (Context funs) name
  = Map.lookup name funs

merge :: Context -> Context -> Context
merge (Context a) (Context b)
  = Context $ Map.union a b

merges :: [Context] -> Context
merges = foldl' merge empty

singleton :: Name -> Scheme -> Context
singleton name scheme
  = Context $ Map.singleton name (fun scheme)

names :: Context -> [Name]
names (Context funs)
  = Map.keys funs

fromList :: [(Name, Fun)] -> Context
fromList = Context . Map.fromList

toList :: Context -> [(Name, Fun)]
toList (Context funs) = Map.toList funs

instance Monoid Context where
  mempty = empty
  mappend = merge
