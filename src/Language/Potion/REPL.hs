module Language.Potion.REPL (repl) where

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
import Language.Potion.Typing

newtype REPLState = RS
  { typectx :: Context
  }

type REPL a = HaskelineT (StateT REPLState IO) a

hoistError :: Show e => Either e a -> REPL a
hoistError (Right val) = return val
hoistError (Left error) = do
  liftIO $ print error
  abort

exec :: Bool -> String -> REPL ()
exec update source =
  do
    st <- get
    expr <- hoistError $ parseExpression source
    liftIO $ print $ typectx st
    liftIO $ print expr
    typectx' <- hoistError $ inferDecl (typectx st) [("it", expr)]
    let st' = st { typectx = typectx' <> typectx st }
    liftIO $ print typectx'
    when update (put st')
    liftIO $ print $ typeof (typectx st') "it"

cmd :: String -> REPL ()
cmd = exec True

completer = Word (listWordCompleter [])

shell :: REPL a -> IO ()
shell pre
  = flip evalStateT init $ evalRepl "Potion> " cmd [] completer pre
  where
    init = RS emptyContext

repl :: IO ()
repl = shell $ return ()
