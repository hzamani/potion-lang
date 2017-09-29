module Language.Potion.Expand (expand) where

import Language.Potion.Syntax

expand :: Expression -> Expression
expand (EApp (EN (UN "%block%")) exprs) = expandLet (map expand exprs)
expand (EApp (EN (UN "if")) [p, t, f]) = expandIf p t f
expand (EApp f args) = EApp (expand f) (map expand args)
-- expand (EMatch expr clauses) = EMatch (expand expr) (map expandWith clauses)
-- expand (EFun [EPlace] body) = EFun [it] (replace EPlace it body)
expand (EFun params expr) = EFun params (expand expr)
expand expr = expr

expandLet :: [Expression] -> Expression
expandLet [expr] = expr
expandLet (EApp (EN (UN "=")) [var, val] : rest) = EApp (en "%let%") [var, val, expandLet rest]
expandLet (head : rest) = EApp (en "%block%") [head, expandLet rest]

expandIf :: Expression -> Expression -> Expression -> Expression
expandIf predicate onTrue onFalse =
  EMatch predicate
    [ (true, EPlace, onTrue)
    , (false, EPlace, onFalse)
    ]

