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
type Pac = (PackageName, UserName, Scheme)

data Context
  = Context (Map UserName Fun) (Map UserName Pac)
  deriving (Eq, Show)

insertApp :: UserName -> Type -> Apps -> Apps
insertApp name ty
  = Map.insertWith Set.union name (Set.singleton ty)

mun :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Map k (Set a)
mun = Map.unionWith Set.union

muns :: (Ord k, Ord a) => [Map k (Set a)] -> Map k (Set a)
muns = Map.unionsWith Set.union

empty :: Context
empty = Context Map.empty Map.empty

noApp :: Apps
noApp = Map.empty

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
extend (Context funs pacs) (name, scheme)
  = Context (Map.insert name (fun scheme) funs) pacs

extends :: Context -> [(UserName, Scheme)] -> Context
extends (Context funs pacs) vars
  = Context (Map.union (Map.map fun $ Map.fromList vars) funs) pacs

extendPacked :: Context -> (UserName, Pac) -> Context
extendPacked (Context funs pacs) (name, pac)
  = Context funs (Map.insert name pac pacs)

remove :: Context -> UserName -> Context
remove (Context funs pacs) name
  = Context (Map.delete name funs) pacs

lookup :: Context -> UserName -> Maybe Fun
lookup (Context funs _) name
  = Map.lookup name funs

merge :: Context -> Context -> Context
merge (Context fa pa) (Context fb pb)
  = Context (Map.union fa fb) (Map.union pa pb)

merges :: [Context] -> Context
merges = foldl' merge empty

singleton :: UserName -> Scheme -> Context
singleton name scheme
  = Context (Map.singleton name (fun scheme)) Map.empty

names :: Context -> [UserName]
names (Context funs _)
  = Map.keys funs

fromList :: [(UserName, Fun)] -> Context
fromList funs = Context (Map.fromList funs) Map.empty

toList :: Context -> [(UserName, Fun)]
toList (Context funs _) = Map.toList funs

expandAliases :: Context -> Definition -> Definition
expandAliases (Context _ pacs) (defName, exp)
  = (defName, walk expand exp)
  where
    expand e@(EN (UN name))
      = case Map.lookup name pacs of
          Just (package, func, _) -> EN (UN $ qname package func)
          Nothing -> e
    expand e = e

qname :: String -> String -> String
qname "" func = func
qname package func = package ++ "." ++ func

generatePackage :: Context -> [Definition] -> Package
generatePackage ctx@(Context _ pacs) defs
  = Package imports (map (expandAliases ctx) defs)
  where
    imports = map Import $ filter (/= "") $ map fst3 $ Map.elems pacs
    fst3 (x, _, _) = x

instance Monoid Context where
  mempty = empty
  mappend = merge
