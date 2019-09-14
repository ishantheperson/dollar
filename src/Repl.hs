{-# LANGUAGE LambdaCase #-}
module Repl where 

import AST

import Eval
import Eval.Context
import Eval.C0Value 

import Parser
--import Parser.Expression 
import Parser.Lexer 

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Foldable (for_)
import Data.Functor ((<&>))

import Control.Monad.State
import Control.Monad.Trans.Except

import System.Console.Haskeline

--repl :: [Function] -> IO () 
repl fs = runEvalT $ runInputT settings (loop fs)

loop :: [Function] -> InputT Evaluator ()
loop fs = do 
  getInputLine "$> " >>= \case
    Nothing -> return ()
    Just "#quit" -> return ()
    Just input -> do 
      let parseResult = test' replParser input 
      result <- lift $ case parseResult of 
                  Left e -> evalE fs e 
                  Right stmnt -> C0VoidVal <$ (runExceptT $ evalS fs stmnt)

      outputStrLn $ show result 
      loop fs

-- Eventually we will change this to another StateT monad,
-- this one keeping track of functions. Then we can also
-- get TAB completion for functions instead of just for
-- variables like we do now
settings :: Settings (Evaluator)
settings = setComplete completer (defaultSettings { historyFile = Just ".dollarHistory" })
  where completer = completeWord Nothing " \t" findCompletion

findCompletion :: String -> Evaluator [Completion] 
findCompletion s = do 
  vars <- gets getAllVars
  return $ mapMaybe (\v -> if s `isPrefixOf` v  
                             then Just $ simpleCompletion v
                             else Nothing) (map varName vars)
