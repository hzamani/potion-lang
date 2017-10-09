module Language.Potion.REPL (repl) where

import Control.Monad.State.Strict
import Data.Monoid
import System.Exit
import System.Environment
import System.Console.Haskeline

import Language.Potion.Parser
import Language.Potion.Expand
import Language.Potion.Type.Context
import Language.Potion.Type.Infer

settings :: Settings IO
settings
  = defaultSettings{ historyFile = Just ".potion_history" }

repl :: IO ()
repl = runInputT settings $ loop defaultPrompt

defaultPrompt = getInputLine "Potion> "
continuePrompt old = getInputLineWithInitial "      | " (old, "")

loop reader
  = do
    line <- reader
    case line of
      Nothing ->
        return ()
      Just "" ->
        loop defaultPrompt
      Just str -> do
        case parseExp str of
          Left e -> outputStrLn $ show e
          Right exp -> outputStrLn $ show $ inferExp (expandExp exp) base
        loop defaultPrompt
          -- Right exp -> do
          --   outputStrLn (show exp)
          --   loop defaultPrompt
          -- Left err  -> do
          --   outputStrLn (show err)
          --   loop $ continuePrompt str
