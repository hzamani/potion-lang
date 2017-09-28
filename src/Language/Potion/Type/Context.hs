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
type Apps = Map UserName (Set Type)
type Fun = (Scheme, Apps, Variants)

newtype Context
  = Context (Map UserName Fun)
  deriving (Eq, Show)

insertApp :: UserName -> Type -> Apps -> Apps
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
  , ("println", fun $ Forall [av] (tFun a unit))
  ]
  where
    av = TVar "a"
    a = TV av
    aa = tTuple [a, a]
    unit = tTuple []
    int = TN "Int"
    bin = fun $ Forall [av] (tFun aa a)
    bool = fun $ Forall [av] (tFun aa (TN "Bool"))

fun :: Scheme -> Fun
fun scheme = (scheme, Map.empty, Set.empty)

extend :: Context -> (UserName, Scheme) -> Context
extend (Context funs) (name, scheme)
  = Context $ Map.insert name (fun scheme) funs

extends :: Context -> [(UserName, Scheme)] -> Context
extends (Context funs) vars
  = Context $ Map.union (Map.map fun $ Map.fromList vars) funs

remove :: Context -> UserName -> Context
remove (Context funs) name
  = Context $ Map.delete name funs

lookup :: Context -> UserName -> Maybe Fun
lookup (Context funs) name
  = Map.lookup name funs

merge :: Context -> Context -> Context
merge (Context a) (Context b)
  = Context $ Map.union a b

merges :: [Context] -> Context
merges = foldl' merge empty

singleton :: UserName -> Scheme -> Context
singleton name scheme
  = Context $ Map.singleton name (fun scheme)

names :: Context -> [UserName]
names (Context funs)
  = Map.keys funs

fromList :: [(UserName, Fun)] -> Context
fromList = Context . Map.fromList

toList :: Context -> [(UserName, Fun)]
toList (Context funs) = Map.toList funs

instance Monoid Context where
  mempty = empty
  mappend = merge
