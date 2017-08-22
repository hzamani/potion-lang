module Main where

import Language.Potion.REPL

main :: IO ()
main = repl

-- import Language.Potion.Parser
-- import Language.Potion.Syntax
-- import Language.Potion.Typing

-- import Control.Monad.Trans
-- import System.Console.Haskeline

-- process :: Show a => Either a Expression -> IO ()
-- process result =
--   case result of
--     Left err -> print err
--     Right exp -> print (exp, judge Empty exp)

-- main :: IO ()
-- main = runInputT defaultSettings (loop "")
--   where
--     loop last =
--       do
--         input <- getInputLine (if last == "" then "Potion> " else "      | ")
--         case input of
--           Nothing -> outputStrLn "Bye."
--           Just line ->
--             let
--               (rest, result) =
--                 case parseExpression (last ++ line) of
--                   Left error -> (line, Left error)
--                   Right exp  -> ("", Right exp)
--             in
--               liftIO (process result) >> loop rest

