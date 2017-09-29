module Language.Potion (compile) where

import Data.Either.Combinators (mapRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Exit
import Text.PrettyPrint

import Language.Potion.Syntax
import Language.Potion.Codegen
import Language.Potion.Expand
import Language.Potion.Parser
import Language.Potion.Type
import Language.Potion.Type.Infer
import Language.Potion.Type.Context
import qualified Language.Potion.Type.Context as Context
import Language.Potion.Type.Substitution

hoistError :: Show e => Either e a -> IO a
hoistError (Right val) = return val
hoistError (Left err) = do
  print err
  exitFailure

compile :: String -> IO ()
compile file =
  do
    code <- readFile file
    source <- hoistError $ parseFile file code
    -- putStrLn "-----------------------------------------------"
    -- print source
    (ctx, defs) <- hoistError $ compileFile Context.base source
    -- putStrLn "-----------------------------------------------"
    -- print ctx
    -- putStrLn "-----------------------------------------------"
    -- print defs
    -- putStrLn "-----------------------------------------------"
    putStrLn $ toGo $ generatePackage ctx defs


compileFile :: Context -> SourceFile -> Either InferError (Context, [Definition])
compileFile ctx (File decls)
  = mapRight finalize $ foldl step (Right (ctx, [])) decls
  where
    finalize (ctx, defs) = (ctx, unifyDefs ctx $ reverse defs)
    step (Right (ctx, defs)) f@DForeign{}
      = Right (extendForeign ctx f, defs)
    step (Right (ctx, defs)) (DDef name params body)
      = case inferExp ctx $ expand $ EFun params body of
          Left err -> Left err
          Right (expr, scheme, apps) -> Right (extendApps ctx (un name, scheme, apps), (name, expr):defs)
    step ctx _ = ctx

extendApps :: Context -> (UserName, Scheme, Apps) -> Context
extendApps (Context funs pacs) (name, scheme, apps)
  = Context (Map.insert name (scheme, apps, Set.empty) funsWithVariants) pacs
  where
    funsWithVariants = Map.foldlWithKey insertVariants funs apps

    insertVariants :: Map UserName Fun -> UserName -> Set Type -> Map UserName Fun
    insertVariants funs name ty = Map.adjust (insertVariant ty) name funs

    insertVariant :: Set Type -> Fun -> Fun
    insertVariant ty (sc, as, vs) = (sc, as, Set.filter noFree ty `Set.union` vs)

    noFree = (0 ==) . Set.size . free

extendForeign :: Context -> Declaration -> Context
extendForeign ctx (DForeign qualified (UN alias) ty)
  = case split '#' qualified of
      (package@(_:_), name@(_:_)) ->
        extendPacked ctx (alias, (package, name, generalize ctx ty))
      (name@(_:_), "") ->
        extendPacked ctx (alias, ("", name, generalize ctx ty))
      _ ->
        error $ "Invalid qualified name: " ++ show qualified
extendForeign ctx _ = ctx

split :: (Eq a) => a -> [a] -> ([a], [a])
split char str
  = spliter char str []
  where
    spliter char [] left
      = (reverse left, [])
    spliter char (c:right) left
      | char == c
      = (reverse left, right)
    spliter char (c:right) left
      = spliter char right (c:left)

