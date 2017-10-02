module Language.Potion.REPL (repl) where

import Control.Monad.State.Strict
import Data.Monoid
import System.Exit
import System.Environment
import System.Console.Haskeline

import Language.Potion.Parser

settings :: Settings IO
settings
  = defaultSettings{ historyFile = Just ".potion_history" }

repl :: IO ()
repl = runInputT settings $ loop defaultPrompt

defaultPrompt = getInputLine "Potion> "
continuePrompt old = getInputLineWithInitial "      | " (old, "")

loop reader
  = do
    row <- reader
    case row of
      Nothing -> return ()
      Just "" ->
        loop defaultPrompt
      Just str ->
        case parseExpression str of
          Right exp -> do
            outputStrLn (show exp)
            loop defaultPrompt
          Left err  -> do
            outputStrLn (show err)
            loop $ continuePrompt str
