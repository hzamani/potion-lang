module Language.Potion (compile) where

import System.Exit
import Text.PrettyPrint
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax
import Language.Potion.Codegen
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
    (ctx, defs) <- hoistError $ inferDecl Ctx.base decls
    -- putStrLn "-----------------------------------------------"
    -- print ctx
    -- putStrLn "-----------------------------------------------"
    -- print defs
    -- putStrLn "-----------------------------------------------"
    putStrLn $ toGo ctx defs

toNameExprPair :: [Declaration] -> [(Name, Expression)]
toNameExprPair
  = concatMap toExpr
  where
    toExpr (DDef name params body) = [(name, expand $ EFun params body)]
    toExpr _ = []

