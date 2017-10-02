module Language.Potion (compile) where

import System.Exit

import Language.Potion.Parser

exitOnError :: Show e => Either e a -> IO a
exitOnError (Right val)
  = return val
exitOnError (Left err)
  = do
    print err
    exitFailure

compile :: String -> IO ()
compile file =
  do
    source <- readFile file
    code <- exitOnError $ parseFile file source
    putStrLn "-----------------------------------------------"
    print code
    putStrLn "-----------------------------------------------"
