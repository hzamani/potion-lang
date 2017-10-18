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

process :: String -> IO ()
process line
  = case parseExp line of
      Left err  -> print err
      Right exp -> do
        print exp
        print $ inferExp (expandExp exp) base

loop reader
  = do
    line <- reader
    case line of
      Nothing ->
        return ()
      Just "" ->
        loop defaultPrompt
      Just input -> do
        liftIO $ process input
        loop defaultPrompt
