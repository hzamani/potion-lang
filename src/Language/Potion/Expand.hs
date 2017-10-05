module Language.Potion.Expand
  ( expand
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.Potion.Syntax

expand :: Code -> Code
expand (Code file defs)
  = Code file $ Map.map expandDef defs

expandDef :: Definition -> Definition
expandDef (DFun meta exp)
  = DFun meta $ expandExp exp
expandDef def
  = def

expandExp :: Expression -> Expression
expandExp = eWalk expander

expander :: Expression -> Expression
expander (EApp _ (EName _ "%block%") exps)
  = expandBlock exps
expander exp
  = exp

expandBlock :: [Expression] -> Expression
expandBlock [exp]
  = exp
expandBlock (EApp ma (EName mn "=") [name, val] : rest)
  = EApp ma (EName mn "%let%") [name, val, expandBlock rest]
expandBlock (head : rest)
  = EApp (ePos head) (eName "%then%") [head, expandBlock rest]

