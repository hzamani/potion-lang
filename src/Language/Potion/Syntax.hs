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

data Pos
  = NoPos
  | Pos SourcePos
  deriving (Eq)

instance Show Pos where
  show NoPos = ""
  show (Pos pos)
    = show (sourceName pos) ++ "@" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)

data Expression
  = EApp Pos Expression [Expression]
  | EFun Pos [Expression] Expression
  | EMatch Pos Expression [Expression]
  | ECase Pos Expression Expression Expression -- with when exp
  | ELit Pos Literal
  | EName Pos Name
  | EHole Pos
  deriving (Eq)

instance Show Expression where
  show (EApp _ (EName _ "%let%") [x, val, exp])
    = show x ++ " = " ++ show val ++ " ; " ++ show exp
  show (EApp _ (EName _ "%then%") [e1, e2])
    = show e1 ++ " ; " ++ show e2
  show (EApp _ f args)
    = "(" ++ show f ++ " " ++ show args ++ ")"
  show (EFun _ params exp)
    = "(λ" ++ show params ++ " -> " ++ show exp ++ ")"
  show (EMatch _ exp cases)
    = "(μ " ++ show exp ++ " " ++ show cases ++ ")"
  show (ECase _ with when exp)
    = "(ω " ++ show (with, when) ++ " => " ++ show exp ++ ")"
  show (ELit _ val)
    = show val
  show (EName _ name)
    = name
  show (EHole _)
    = "◻"

data Definition
  = DFun Pos Expression
  | DSig Pos Type
  | DForeign Pos String Type
  deriving (Eq, Show)

data Code
  = Code Name (Map Name Definition)
  deriving (Eq, Show)

ePos :: Expression -> Pos
ePos (EApp pos _ _)    = pos
ePos (EFun pos _ _)    = pos
ePos (EMatch pos _ _)  = pos
ePos (ECase pos _ _ _) = pos
ePos (ELit pos _)      = pos
ePos (EName pos _)     = pos
ePos (EHole pos)       = pos

putPos :: Pos -> Expression -> Expression
putPos p (EApp _ f as)   = EApp p f as
putPos p (EFun _ ps e)   = EFun p ps e
putPos p (EMatch _ f as) = EMatch p f as
putPos p (ECase _ x y z) = ECase p x y z
putPos p (ELit _ v)      = ELit p v
putPos p (EName _ n)     = EName p n
putPos p (EHole _)       = EHole p

eApp   = EApp NoPos
eFun   = EFun NoPos
eMatch = EMatch NoPos
eCase  = ECase NoPos
eLit   = ELit NoPos
eName  = EName NoPos
eHole  = EHole NoPos

eOp op = eApp (eName op)

eTrue  = eName "true"
eFalse = eName "false"

dFun     = DFun NoPos
dSig     = DSig NoPos
dForeign = DForeign NoPos

eWalk :: (Expression -> Expression) -> Expression -> Expression
eWalk f (EApp pos exp args)
  = f $ EApp pos (eWalk f exp) (map (eWalk f) args)
eWalk f (EFun pos params body)
  = f $ EFun pos (map (eWalk f) params) (eWalk f body)
eWalk f (EMatch pos exp cases)
  = f $ EMatch pos (eWalk f exp) (map (eWalk f) cases)
eWalk f (ECase pos with when exp)
  = f $ ECase pos (eWalk f with) (eWalk f when) (eWalk f exp)
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

