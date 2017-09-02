module Main where

import System.Environment
import Language.Potion
import Language.Potion.REPL

main :: IO ()
main
  = do
    args <- getArgs
    case args of
      [] -> repl
      file:_ -> compile file

