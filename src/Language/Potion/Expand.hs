module Language.Potion.Expand (expand) where

import Language.Potion.Syntax

expand :: Expression -> Expression
expand (EApp (EN "do") exprs) = addLets (map expand exprs)
expand (EApp (EN "?") [p, EApp (EN "()") [t, f]]) = ifToMatch p t f
expand (EApp f args) = EApp (expand f) (map expand args)
-- expand (EMatch expr clauses) = EMatch (expand expr) (map expandWith clauses)
expand (EFun [EPlace] body) = EFun [it] (replace EPlace it body)
expand (EFun params expr) = EFun params (expand expr)
expand expr = expr

addLets :: [Expression] -> Expression
addLets [expr] = expr
addLets (EApp (EN "=") [pat, val] : rest) = ELet pat val (addLets rest)
addLets (head : rest) = EApp (EN "do") [head, addLets rest]

ifToMatch :: Expression -> Expression -> Expression -> Expression
ifToMatch predicate onTrue onFalse =
  EMatch predicate
    [ (true, EPlace, onTrue)
    , (false, EPlace, onFalse)
    ]

true = EN "true"
false = EN "false"
it = EN "αυτό"
