module Language.Potion (compile) where

import System.Exit

import Language.Potion.Parser
import Language.Potion.Expand
import Language.Potion.Type.Infer
import Language.Potion.Type.Context (base)

exitOnError :: Show e => Either e a -> IO a
exitOnError (Right val)
  = return val
exitOnError (Left err)
  = do
    print err
    exitFailure

compile :: String -> IO ()
compile file
  = do
    source <- readFile file
    putStrLn "-----------------------------------------------"
    code <- exitOnError $ parse file source
    print code
    putStrLn "-----------------------------------------------"
    let expanded = expand code
    print expanded
    putStrLn "-----------------------------------------------"
    context <- exitOnError $ inferCode expanded base
    print context
    -- putStrLn "-----------------------------------------------"
    -- generated <- exitOnError $ codegen context
    -- print generated
    -- putStrLn "-----------------------------------------------"

