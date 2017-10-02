module Language.Potion.Syntax where

import Debug.Trace
import Data.Map.Strict (Map)
import Text.Parsec.Pos

import Language.Potion.Type

type Name = String

data Literal
  = LB Bool
  | LI Integer
  | LF Double
  | LC Char
  | LS String
  deriving (Eq)

instance Show Literal where
  show (LB x) = show x
  show (LI x) = show x
  show (LF x) = show x
  show (LC x) = show x
  show (LS x) = show x

data Meta
  = Meta
    { metaPos :: Maybe SourcePos
    , metaType :: Maybe Type
    , metaAlias :: Maybe (Name, Name)
    }
  deriving (Eq, Show)

data Expression
  = EApp Meta Expression [Expression]
  | EFun Meta [Expression] Expression
  | EMatch Meta Expression [Expression]
  | ECase Meta Expression Expression Expression -- with when exp
  | ELit Meta Literal
  | EName Meta Name
  | EHole Meta
  deriving (Eq)

instance Show Expression where
  show (EApp _ f args)         = "(" ++ show f ++ " " ++ show args ++ ")"
  show (EFun _ params exp)     = "(λ" ++ show params ++ " -> " ++ show exp ++ ")"
  show (EMatch _ exp cases)    = "(μ " ++ show exp ++ " " ++ show cases ++ ")"
  show (ECase _ with when exp) = "(ω " ++ show (with, when) ++ " => " ++ show exp ++ ")"
  show (ELit _ val)            = show val
  show (EName _ name)          = name
  show (EHole _)               = "◻"

data Definition
  = DFun Meta Expression
  | DSig Meta Type
  | DForeign Meta String Type
  deriving (Eq, Show)

type Code = Map Name Definition

data SourceFile
  = File Name Code
  deriving (Eq, Show)

noMeta :: Meta
noMeta
  = Meta
    { metaPos = Nothing
    , metaType = Nothing
    , metaAlias = Nothing
    }

metaFromPos :: SourcePos -> Meta
metaFromPos p = noMeta{ metaPos = Just p }

eMeta :: Expression -> Meta
eMeta (EApp meta _ _)    = meta
eMeta (EFun meta _ _)    = meta
eMeta (EMatch meta _ _)  = meta
eMeta (ECase meta _ _ _) = meta
eMeta (ELit meta _)      = meta
eMeta (EName meta _)     = meta
eMeta (EHole meta)       = meta

putMeta :: Meta -> Expression -> Expression
putMeta m (EApp _ f as)   = EApp m f as
putMeta m (EFun _ ps e)   = EFun m ps e
putMeta m (EMatch _ f as) = EMatch m f as
putMeta m (ECase _ x y z) = ECase m x y z
putMeta m (ELit _ v)      = ELit m v
putMeta m (EName _ n)     = EName m n
putMeta m (EHole _)       = EHole m

eApp   = EApp noMeta
eFun   = EFun noMeta
eMatch = EMatch noMeta
eCase  = ECase noMeta
eLit   = ELit noMeta
eName  = EName noMeta
eHole  = EHole noMeta

eOp op = eApp (eName op)

eTrue  = eName "true"
eFalse = eName "false"

dFun     = DFun noMeta
dSig     = DSig noMeta
dForeign = DForeign noMeta

-- typeof :: Expression -> Type
-- typeof = metaType . eMeta

eWalk :: (Expression -> Expression) -> Expression -> Expression
eWalk f (EApp meta exp args)
  = f $ EApp meta (eWalk f exp) (map (eWalk f) args)
eWalk f (EFun meta params body)
  = f $ EFun meta (map (eWalk f) params) (eWalk f body)
eWalk f (EMatch meta exp cases)
  = f $ EMatch meta (eWalk f exp) (map (eWalk f) cases)
eWalk f (ECase meta with when exp)
  = f $ ECase meta (eWalk f with) (eWalk f when) (eWalk f exp)
eWalk f exp
  = f exp

eReplace :: Expression -> Expression -> Expression -> Expression
eReplace x y
  = let rep exp = if exp == x then y else exp in eWalk rep

lType :: Literal -> Type
lType (LB _) = tBool
lType (LI _) = tInt
lType (LF _) = tFloat
lType (LC _) = tChar
lType (LS _) = tString


debug :: Show a => String -> a -> a
debug msg x = trace (msg ++ " " ++ show x) x

