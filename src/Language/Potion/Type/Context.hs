module Language.Potion.Type.Context where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Core
import Language.Potion.Syntax
import Language.Potion.Type

data Info
  = Info
    { variants :: Set Type
    , package :: Name
    , alias :: Name
    , apps :: Map Name (Set Type)
    , scheme :: Scheme
    }

fromScheme :: Scheme -> Info
fromScheme scheme
  = Info
    { variants = Set.empty
    , package = ""
    , alias = ""
    , apps = Map.empty
    , scheme = scheme
    }

insertApp :: Name -> Type -> Info -> Info
insertApp name ty info
  = info{ apps = newApps }
  where
    oldApps = apps info
    newApps = Map.insertWith Set.union name (Set.singleton ty) oldApps

unionApps :: [CExp] -> Map Name (Set Type)
unionApps exps
  = Map.unionsWith Set.union $ map cApps exps

type Context = Map Name Info

singleton :: Name -> Scheme -> Context
singleton name scheme
  = Map.singleton name (fromScheme scheme)

fromList :: [(Name, Scheme)] -> Context
fromList = Map.map fromScheme . Map.fromList

extend :: (Name, Scheme) -> Context -> Context
extend (name, scheme)
  = Map.insert name (fromScheme scheme)

extends :: [(Name, Scheme)] -> Context -> Context
extends defs ctx
  = union ctx $ fromList defs

lookup :: Name -> Context -> Maybe Info
lookup = Map.lookup

union :: Context -> Context -> Context
union = Map.union

base :: Context
base = fromList
  [ ("+", bin) , ("-", bin) , ("*", bin) , ("/", bin) , ("%", bin)
  , (">", bool) , (">=", bool) , ("==", bool) , ("!=", bool) , ("<", bool) , ("<=", bool)
  , ("println", Forall [av] (tFun a unit))
  ]
  where
    av = TVar "a"
    a = TV av
    aa = tTuple [a, a]
    unit = tTuple []
    bin = Forall [av] (tFun aa a)
    bool = Forall [av] (tFun aa tBool)
