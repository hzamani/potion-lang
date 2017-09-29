module Language.Potion (compile) where

import System.Exit
import Text.PrettyPrint
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax
import Language.Potion.Codegen
import Language.Potion.Expand
import Language.Potion.Parser
import Language.Potion.Type
import Language.Potion.Type.Infer
import Language.Potion.Type.Context
import qualified Language.Potion.Type.Context as Context

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
    (ctx, defs) <- hoistError $ inferFile Context.base source
    -- putStrLn "-----------------------------------------------"
    -- print ctx
    -- putStrLn "-----------------------------------------------"
    -- print defs
    -- putStrLn "-----------------------------------------------"
    putStrLn $ toGo $ generatePackage ctx defs

