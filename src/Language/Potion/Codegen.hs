module Language.Potion.Codegen where

import Data.List
import Text.PrettyPrint

import Language.Potion.Type
import Language.Potion.Syntax

goExp :: Expression -> Doc
goExp (EN name) = text name
goExp (EL x) = goLit x
goExp (ELet pat val exp) = hsep [goExp pat, text ":=", goExp val] $+$ goExp exp
goExp (EApp f [x, y]) | isInfix f = hsep [goExp x, goExp f, goExp y]
goExp (EApp f args) = goExp f <> tuple args
goExp (EFun ps exp) =
  hsep [func, tuple ps, lbrace]
  $+$ nest 4 (goExp exp)
  $+$ rbrace
goExp (EMatch exp branches) = text "switch"

tuple = parens . commaSep . map goExp
commaSep = hsep . punctuate comma

goLit :: Literal -> Doc
goLit (LB True) = text "true"
goLit (LB False) = text "false"
goLit (LI n) = text $ show n
goLit (LF x) = text $ show x
goLit (LC c) = quotes $ char c
goLit (LS s) = doubleQuotes $ text s

goDef :: Declaration -> Type -> Doc
goDef (DDef name params body) (TApp (TN "Fun") [paramTypes, outType])
  = hsep [func, text name, goParams params paramTypes, goType outType, lbrace]
    $+$ nest 4 (goBody body)
    $+$ rbrace

goParams :: [Expression] -> Type -> Doc
goParams xs (TApp (TN "Tuple") ts)
  = parens $ commaSep $ zipWith goParam xs ts
  where
    goParam x t = goExp x <+> goType t

goBody :: Expression -> Doc
goBody (EApp (EN "do") es) = body es
goBody exp = body [exp]

body [] = empty
body [x] = text "return" <+> goExp x
body (x:xs) = goExp x $+$ body xs

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
isInfix (EN "+") = True
isInfix (EN "-") = True
isInfix (EN "*") = True
isInfix (EN "=") = True
isInfix _ = False
