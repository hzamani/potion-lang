module Language.Potion.Codegen where

import Data.Char (isAlpha)
import Data.List
import Text.PrettyPrint

import Language.Potion.Type
import Language.Potion.Syntax

goExp :: [Expression] -> Expression -> Doc
goExp ns@(_:_) (ET exp@(EMatch _ _) ty)
  = goExp ns exp
  $+$ text "var unreachable" <+> goType ty
  $+$ text "return unreachable"
goExp ns (ET exp ty) = goExp ns exp
goExp ns (EMatch exp branches)
  = hsep [text "switch", goExp [] exp, lbrace]
  $+$ nest 4 (goCases ns branches)
  $+$ rbrace
goExp ns (EApp (ET (EN "recur") _) args)
  = hsep [tuple' ns, text "=", tuple' args]
  $+$ text "goto start"
goExp (_:_) exp = text "return" <+> goExp [] exp
goExp ns (EN name) = text name
goExp ns (EL x) = goLit x
goExp ns (EApp f [x, y]) | isInfix f = hsep [goExp [] x, goExp [] f, goExp [] y]
goExp ns (EApp f args) = goExp [] f <> tuple args
goExp ns (EFun ps exp) =
  hsep [func, tuple ps, lbrace]
  $+$ nest 4 (goBody ps exp)
  $+$ rbrace
goExp _ e = error $ "goExp not implemented for: " ++ show e

goCases ns
  = vcat . map goCase
  where
    goCase (cond, ET EPlace _, exp) = text "case" <+> goExp [] cond <> text ":" $+$ nest 4 (goExp ns exp)
    goCase x = text $ show x

tuple = parens . commaSep . map (goExp [])
tuple' = commaSep . map (goExp [])
commaSep = hsep . punctuate comma

goLit :: Literal -> Doc
goLit (LB True) = text "true"
goLit (LB False) = text "false"
goLit (LI n) = text $ show n
goLit (LF x) = text $ show x
goLit (LC c) = quotes $ char c
goLit (LS s) = doubleQuotes $ text s

goDef :: Definition -> Doc
goDef (name, ET (EFun params expr) (TApp (TN "Fun") [paramTypes, outType]))
  = hsep [func, text name, goParams params paramTypes, goType outType, lbrace]
    $+$ nest 4 (goBody params expr)
    $+$ rbrace
goDef e = error $ "goDef not implemented for: " ++ show e

-- goDef :: Declaration -> Type -> Doc
-- goDef (DDef name params body) (TApp (TN "Fun") [paramTypes, outType])
--   = hsep [func, text name, goParams params paramTypes, goType outType, lbrace]
--     $+$ nest 4 (goBody body)
--     $+$ rbrace

goParams :: [Expression] -> Type -> Doc
goParams xs (TApp (TN "Tuple") ts)
  = parens $ commaSep $ zipWith goParam xs ts
  where
    goParam x t = goExp [] x <+> goType t

goBody :: [Expression] -> Expression -> Doc
-- goBody (EApp (EN "%block%") es) = body es
goBody params exp = label $+$ body params [exp]
  where
    label =
      if isRecur exp
         then text "start:"
         else empty

isRecur (ET expr _) = isRecur expr
isRecur (EN "recur") = True
isRecur (EApp expr _) = isRecur expr
isRecur (EMatch _ cases) = any (\(_, _, e) -> isRecur e) cases
isRecur (EFun _ expr) = isRecur expr
isRecur _ = False

body :: [Expression] -> [Expression] -> Doc
body ps [] = empty
-- body ps [ET e _] = body ps [e]
-- body ps [EApp (ET (EN "recur") _) es] = hsep [tuple' ps, text ":=", tuple' es]
body ps [ET (EApp (EN "%block%") es) _] = body ps es
body ps [ET (EApp (EN "%let%") [x, v, e]) _] = hsep [goExp [] x, text ":=", goExp [] v] $+$ body ps [e]
-- body ps e = error $ "BODY: " ++ show e
body ps [x] = goExp ps x
body ps (x:xs) = goExp [] x $+$ body ps xs

goType :: Type -> Doc
goType (TN "Bool") = text "bool"
goType (TN "Int") = text "int"
goType (TN "Float") = text "float64"
goType (TN "Char") = text "rune"
goType (TN "String") = text "string"
goType (TApp (TN "Tuple") [t]) = goType t
goType (TApp (TN "Tuple") ts) = parens $ hsep $ punctuate comma $ map goType ts
goType (TApp (TN "List") [a]) = brackets $ goType a
goType (TApp (TN "Fun") [a, b]) = hsep [func, parens $ goType a, goType b]
goType (TApp (TN "Map") [a, b]) = hcat [text "map", brackets $ goType a, goType b]
goType (TV _) = error "can't use type vars in go!"

func = text "func"

isInfix :: Expression -> Bool
isInfix (ET e _) = isInfix e
isInfix (EN (x:_)) = not $ isAlpha x
isInfix e = error $ "not for: " ++ show e
