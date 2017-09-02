module Language.Potion (compile) where

import System.Exit

import Language.Potion.Syntax
import Language.Potion.Expand
import Language.Potion.Parser
import Language.Potion.Type
import Language.Potion.Type.Infer
import qualified Language.Potion.Type.Context as Ctx

hoistError :: Show e => Either e a -> IO a
hoistError (Right val) = return val
hoistError (Left err) = do
  print err
  exitFailure

compile :: String -> IO ()
compile file =
  do
    source <- readFile file
    decls <- hoistError $ parseFile file source
    print decls
    ctx <- hoistError $ inferDecl Ctx.base $ toNameExprPair decls
    print ctx

toNameExprPair :: [Declaration] -> [(Name, Expression)]
toNameExprPair
  = concatMap toExpr
  where
    toExpr (DDef name params body) = [(name, expand $ EFun params body)]
    toExpr _ = []

