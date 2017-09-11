module Language.Potion.Expand (expand) where

import Language.Potion.Syntax

import Debug.Trace

debug message val = trace (message ++ show val) val

expand :: Expression -> Expression
expand (EApp (EN "%block%") exprs) = expandLet (map expand exprs)
expand (EApp (EN "if") [p, t, f]) = expandIf p t f
expand (EApp f args) = EApp (expand f) (map expand args)
-- expand (EMatch expr clauses) = EMatch (expand expr) (map expandWith clauses)
-- expand (EFun [EPlace] body) = EFun [it] (replace EPlace it body)
expand (EFun params expr) = EFun params (expand expr)
expand expr = trace ("EXPAND NO OP: " ++ show expr) expr

expandLet :: [Expression] -> Expression
expandLet [expr] = expr
expandLet (EApp (EN "=") [var, val] : rest) = EApp (EN "%let%") [var, val, expandLet rest]
expandLet (head : rest) = EApp (EN "%block%") [head, expandLet rest]

expandIf :: Expression -> Expression -> Expression -> Expression
expandIf predicate onTrue onFalse =
  EMatch predicate
    [ (true, EPlace, onTrue)
    , (false, EPlace, onFalse)
    ]

true = EN "true"
false = EN "false"
it = EN "αυτό"
