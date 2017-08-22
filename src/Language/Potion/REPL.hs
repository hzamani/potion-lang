module Language.Potion.REPL (repl) where

import Prelude hiding (lookup)

import Control.Monad.State.Strict
import Data.Monoid
-- import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.Environment
import System.Console.Repline

import Language.Potion.Parser
import Language.Potion.Syntax
import Language.Potion.Type
import Language.Potion.Type.Context
import Language.Potion.Type.Infer

newtype REPLState = RS Context

type REPL a = HaskelineT (StateT REPLState IO) a

hoistError :: Show e => Either e a -> REPL a
hoistError (Right val) = return val
hoistError (Left error) = do
  liftIO $ print error
  abort

exec :: Bool -> String -> REPL ()
exec update source =
  do
    (RS ctx) <- get
    expr <- hoistError $ parseExpression source
    liftIO $ print expr
    -- liftIO $ print ctx
    -- liftIO $ print $ constraintsExpr ctx expr
    ctx' <- hoistError $ inferDecl ctx [("it", expr)]
    let st = RS $ ctx' <> ctx
    when update (put st)
    liftIO $ print $ lookup ctx' "it"

cmd :: String -> REPL ()
cmd = exec True

completer = Word (listWordCompleter [])

shell :: REPL a -> IO ()
shell pre
  = flip evalStateT init $ evalRepl "Potion> " cmd [] completer pre
  where
    init = RS base

repl :: IO ()
repl = shell $ return ()
