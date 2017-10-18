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
import Language.Potion.Type.Substitution

data Def
  = Def
    { dScheme :: Scheme
    , dExp :: CExp
    , dVariants :: Set Type
    }
  deriving (Eq)

instance Substitutable Def where
  apply sub def@Def{ dScheme = scheme, dExp = exp } = def{ dScheme = apply sub scheme, dExp = apply sub exp }
  free = free . dScheme

instance Show Def where
  show Def{ dScheme = scheme, dExp = exp, dVariants = vs }
    = unlines
      [ show scheme
      , show vs
      , show exp
      ]

emptyDef
  = Def
    { dScheme = Forall [] (TN "?")
    , dExp = noExp
    , dVariants = Set.empty
    }

fromScheme :: Scheme -> Def
fromScheme scheme
  = Def
    { dScheme = scheme
    , dExp = noExp
    , dVariants = Set.empty
    }

-- insertApp :: Name -> Type -> Info -> Info
-- insertApp name ty info
--   = info{ apps = newApps }
--   where
--     oldApps = apps info
--     newApps = Map.insertWith Set.union name (Set.singleton ty) oldApps

unionApps :: [CExp] -> Map Name (Set Type)
unionApps exps
  = Map.unionsWith Set.union $ map cApps exps

type Context = Map Name Def

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

lookup :: Name -> Context -> Maybe Def
lookup = Map.lookup

insertDef :: Name -> Def -> Context -> Context
insertDef name def@Def{ dExp = exp } ctx
  = Map.foldlWithKey insertVs (Map.insert name def ctx) (cApps exp)

insertVs :: Context -> Name -> Set Type -> Context
insertVs ctx name vs
  = Map.adjust (insertVariants vs) name ctx

insertVariants :: Set Type -> Def -> Def
insertVariants vs def@Def{ dVariants = vs' }
  = def{ dVariants = Set.filter allBound vs `Set.union` vs' }
  where
    allBound = (0 ==) . Set.size . free

union :: Context -> Context -> Context
union = Map.union

base :: Context
base = fromList
  [ ("+", bin) , ("-", bin) , ("*", bin) , ("/", bin) , ("%", bin)
  , (">", bool) , (">=", bool) , ("==", bool) , ("!=", bool) , ("<", bool) , ("<=", bool)
  , ("println", Forall [av] (tFun a unit))
  , ("%then%", Forall [av, bv] (tFun ab b))
  ]
  where
    av = TVar "a"
    bv = TVar "b"
    a = TV av
    b = TV bv
    aa = tTuple [a, a]
    ab = tTuple [a, b]
    unit = tTuple []
    bin = Forall [av] (tFun aa a)
    bool = Forall [av] (tFun aa tBool)
