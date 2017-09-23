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
    print decls
    -- let pairs = toNameExprPair decls
    -- print pairs
    (ctx, defs) <- hoistError $ inferDecl Ctx.base decls
    -- putStrLn "-----------------------------------------------"
    -- print ctx
    putStrLn "-----------------------------------------------"
    print defs
    putStrLn "-----------------------------------------------"
    -- print $ unifyDefs ctx defs
    putStrLn $ toGo ctx defs
    putStrLn "-----------------------------------------------"

toGo :: Ctx.Context -> [Definition] -> String
toGo ctx defs
  = render $ vcat $ map goDef defs
  -- where
  --   go def@(name, exp)
  --     = vcat $ map goDef def) $ ts name

  --   -- define (name, ET exp scheme) ty =
  --   --   case runSolve [(scheme, ty)] of
  --   --     Left err  -> error (show err)
  --   --     Right sub -> goDef (name, apply sub exp)

  --   -- ts "main" = [tFun (tTuple []) (TN "Int")]
  --   ts name
  --     = case Ctx.lookup ctx name of
  --       Just (Forall [] ty, _, variants) -> ty : Set.toList variants
  --       Just (_, _, variants) -> Set.toList variants
  --       Nothing -> []

toNameExprPair :: [Declaration] -> [(Name, Expression)]
toNameExprPair
  = concatMap toExpr
  where
    toExpr (DDef name params body) = [(name, expand $ EFun params body)]
    toExpr _ = []

