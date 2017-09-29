module Language.Potion.Codegen where

import Data.Char (isAlpha)
import Data.List
import Text.PrettyPrint

import Language.Potion.Type
import Language.Potion.Syntax
import Language.Potion.Type.Context (Context)
import qualified Language.Potion.Type.Context as Context

toGo :: Package -> String
toGo (Package imports defs)
  = render $ vcat $ goHeader imports : map goDef defs

goHeader :: [Import] -> Doc
goHeader imports
  = text "package main"
  $+$ text "import" <+> lparen
  $+$ nest 4 (vcat imps)
  $+$ rparen
  where
    imps = map imp (Import "fmt" : imports)
    imp (Import p) = doubleQuotes (text p)

-- FIXME: return in function with no params
goExp :: [Expression] -> Expression -> Doc
goExp ns@(_:_) (ET exp@(EMatch _ _) ty)
  = goExp ns exp
  $+$ text "panic(\"should never happen\")"
goExp ns (ET (EFun ps exp) (TApp (TN "Fun") [_, ty]))
  = hsep [func, goFunParams ps, goType ty, lbrace]
  $+$ nest 4 (goBody ps exp)
  $+$ rbrace
goExp ns (ET exp ty)
  = goExp ns exp
goExp ns (EMatch exp branches)
  = goMatch ns exp branches
goExp ns (EApp (ET (EN (UN "recur")) _) args)
  = hsep [tuple' ns, text "=", tuple' args]
  $+$ text "goto start"
goExp (_:_) exp
  = text "return" <+> goExp [] exp
goExp ns (EN name)
  = goName name
goExp ns (EL x)
  = goLit x
goExp ns ENothing
  = empty
goExp ns EPlace
  = empty
goExp ns (EApp (ET (EN (PN "println" _)) _) args)
  = text "fmt.Println" <> tuple args
goExp ns (EApp (EN (UN "()")) args)
  = tuple' args
goExp ns (EApp (ET (EN (UN "[]")) ty) args)
  = text "[]" <> goType ty <> literal args
goExp ns (EApp (EN (UN "[:]")) [base, index])
  = goExp ns base <> brackets (goExp ns index)
goExp ns (EApp (EN (UN "[:]")) [base, start, end])
  = goExp ns base <> brackets (goExp ns start <> text ":" <> goExp ns end)
goExp ns (EApp f [x, y])
  | isInfix f = hsep [goExp [] x, goExp [] f, goExp [] y]
goExp ns (EApp (ET (EN name) ty) args)
  = goName name <> tuple args
goExp ns (EApp f args)
  = goExp [] f <> tuple args
goExp _ e = error $ "goExp not implemented for: " ++ show e

goCases ns name
  = vcat . map goCase
  where
    goCase (cond, ET ENothing _, exp) = text "case" <+> goExp [] name <+> text "==" <+> goExp [] cond <> text ":" $+$ nest 4 (body ns [exp])
    goCase (ET ENothing _, cond, exp) = text "case" <+> goExp [] cond <> text ":" $+$ nest 4 (body ns [exp])
    goCase x = text $ show x

goMatch ns exp@(ET EApp{} _) branches
  = hsep [text "switch", goExp [] it <+> text ":=" <+> goExp [] exp <> text ";", lbrace]
  $+$ nest 4 (goCases ns it branches)
  $+$ rbrace

goMatch ns exp branches
  = hsep [text "switch", lbrace]
  $+$ nest 4 (goCases ns it branches)
  $+$ rbrace

literal = braces . commaSep . map (goExp [])
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
  = hsep [func, goName name <> goParams params paramTypes, goType outType, lbrace]
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

goFunParams :: [Expression] -> Doc
goFunParams xs
  = parens $ commaSep $ map goParam xs
  where
    goParam (ET x t) = goExp [] x <+> goType t

goBody :: [Expression] -> Expression -> Doc
-- goBody (EApp (EN "%block%") es) = body es
goBody params exp = label $+$ body params [exp]
  where
    label =
      if isRecur exp
         then text "start:"
         else empty

isRecur (ET exp _) = isRecur exp
isRecur (EN (UN "recur")) = True
isRecur (EApp exp args) = isRecur exp || any isRecur args
isRecur (EMatch _ cases) = any (\(_, _, e) -> isRecur e) cases
isRecur _ = False

body :: [Expression] -> [Expression] -> Doc
body ps [] = empty
-- body ps [ET e _] = body ps [e]
-- body ps [EApp (ET (EN "recur") _) es] = hsep [tuple' ps, text ":=", tuple' es]
body ps [ET (EApp (EN (UN "%block%")) es) _] = body ps es
body ps [ET (EApp (EN (UN "%let%")) [x, v, e]) _] = hsep [goExp [] x, text ":=", goExp [] v] $+$ body ps [e]
-- body ps e = error $ "BODY: " ++ show e
body ps [x] = goExp ps x
body ps (x:xs) = goExp [] x $+$ body ps xs

goType :: Type -> Doc
goType (TN "Bool") = text "bool"
goType (TN "Int") = text "int"
goType (TN "Float") = text "float64"
goType (TN "Char") = text "rune"
goType (TN "String") = text "string"
goType (TApp (TN "Tuple") []) = empty
goType (TApp (TN "Tuple") [t]) = goType t
goType (TApp (TN "Tuple") ts) = parens $ hsep $ punctuate comma $ map goType ts
goType (TApp (TN "Array") [a]) = text "[]" <> goType a
goType (TApp (TN "Map") [a, b]) = hcat [text "map", brackets $ goType a, goType b]
goType (TApp (TN "Fun") [a, b]) = hsep [func, parens $ goType a, goType b]
goType (TV _) = error "can't use type vars in go!"

goName :: Name -> Doc
goName (UN name)
  = text name
goName (PN name@(h:_) ts)
  | isAlpha h
  = text $ intercalate "Ξ" (name : ts)
goName (PN name _)
  = text name

func = text "func"

isInfix :: Expression -> Bool
isInfix (ET e _) = isInfix e
isInfix (EN (UN (x:_))) = not $ isAlpha x
isInfix (EN (PN (x:_) _)) = not $ isAlpha x
isInfix e = error $ "isInfix not implemented for: " ++ show e

