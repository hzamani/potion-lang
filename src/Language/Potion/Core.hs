module Language.Potion.Core where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax hiding (putPos)
import Language.Potion.Type

data Meta
  = Meta
    { mType :: Type
    , mApps :: Map Name (Set Type)
    , mPos :: Pos
    }
  deriving (Eq, Show)

noMeta
  = Meta
    { mType = TN "?"
    , mApps = Map.empty
    , mPos = NoPos
    }

putPos pos meta   = meta{ mPos = pos }
putApps apps meta = meta{ mApps = apps }
putType ty meta   = meta{ mType = ty }

insertApp :: Name -> Type -> Map Name (Set Type) -> Map Name (Set Type)
insertApp name ty = Map.insertWith Set.union name (Set.singleton ty)

fromPos pos = putPos pos noMeta

data CExp
  = CApp Meta CExp [CExp]
  | CFun Meta [CExp] CExp
  | CMatch Meta CExp [CExp]
  | CCase Meta CExp CExp CExp -- with when exp
  | CLit Meta Literal
  | CName Meta Name
  | CHole Meta
  | CForeign Meta Name
  deriving (Eq, Show)

cMeta :: CExp -> Meta
cMeta (CApp m _ _)    = m
cMeta (CFun m _ _)    = m
cMeta (CMatch m _ _)  = m
cMeta (CCase m _ _ _) = m
cMeta (CLit m _)      = m
cMeta (CName m _)     = m
cMeta (CHole m)       = m

cType = mType . cMeta
cApps = mApps . cMeta

noExp = CHole noMeta
