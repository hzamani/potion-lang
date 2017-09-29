module Language.Potion.Expand (expand) where

import Language.Potion.Syntax

expand :: Expression -> Expression
expand = walk expand'
  where
    expand' (EApp (EN (UN "%block%")) exps) = expandLet exps
    expand' (EApp (EN (UN "if")) [p, t, f]) = expandIf p t f
    expand' (EMatch exp branches) = expandMatch exp branches
    expand' exp@(EFun [ENothing] body) = replace ENothing it exp
    expand' exp = exp

expandLet :: [Expression] -> Expression
expandLet [expr] = expr
expandLet (EApp (EN (UN "=")) [var, val] : rest) = EApp (en "%let%") [var, val, expandLet rest]
expandLet (head : rest) = EApp (en "%block%") [head, expandLet rest]

expandIf :: Expression -> Expression -> Expression -> Expression
expandIf predicate onTrue onFalse =
  EMatch predicate
    [ (true, ENothing, onTrue)
    , (false, ENothing, onFalse)
    ]

expandMatch :: Expression -> [(Expression, Expression, Expression)] -> Expression
expandMatch exp branches
  = EMatch exp' (map (expandBranch name) branches)
  where
    (name, exp') = expandMatchExp exp

expandMatchExp :: Expression -> (Expression, Expression)
expandMatchExp exp@(ident@(EN _)) = (ident, exp)
-- expandMatchExp exp = (it, exp)
expandMatchExp exp = (it, EApp (en ":=") [it, exp, ENothing])

expandBranch :: Expression -> (Expression, Expression, Expression) -> (Expression, Expression, Expression)
-- expandBranch (whit, when, exp)
expandBranch ident (EApp (EN (UN "[]")) elems, when, exp)
  = (ENothing, EApp (en op) [EApp (en "length") [ident], intLit n], prependLets ident exp elems)
  where
    op = if hasSpread then ">=" else "=="
    hasSpread = n /= length elems
    noSpread = filter (not . isSpread) elems
    n = length noSpread
expandBranch ident b = b

intLit = EL . LI . toInteger

prependLets ident exp = fst . foldl (prependLet ident) (exp, 0)

prependLet ident (exp, n) (EApp (EN (UN "...")) [ENothing])
  = (exp, n)
prependLet ident (exp, n) (EApp (EN (UN "...")) [elem])
  = (eLet [elem, eSlice [ident, intLit n, ENothing], exp], n)
prependLet ident (exp, n) ENothing
  = (exp, n + 1)
prependLet ident (exp, n) elem
  = (eLet [elem, eSlice [ident, intLit n], exp], n + 1)

arrayLength :: [Expression] -> Integer
arrayLength = toInteger . length . filter isSpread

isSpread (ET exp _) = isSpread exp
isSpread (EApp (EN (UN "...")) _) = True
isSpread _ = False

