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

import Control.Monad.State
import Control.Monad.Trans.Except

import System.Console.Haskeline

--repl :: [Function] -> IO () 
repl fs = runEvalT $ runInputT (settings fs) (loop fs)

loop :: [Function] -> InputT Evaluator ()
loop fs = do 
  getInputLine "$> " >>= \case
    Nothing -> return ()
    Just "#quit" -> return ()
    Just "#functions" -> do 
      forM_ fs (outputStrLn . functionName)
      loop fs 
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
settings :: [Function] -> Settings (Evaluator)
settings fs = setComplete completer (defaultSettings { historyFile = Just ".dollarHistory" })
  where completer = completeWord Nothing " \t" (findCompletion fs)

findCompletion :: [Function] -> String -> Evaluator [Completion] 
findCompletion fs s = do 
  vars <- gets getAllVars
  let funcNames = map functionName fs 
      varNames = map varName vars 

      names = funcNames ++ varNames 
  return $ mapMaybe (\v -> if s `isPrefixOf` v  
                             then Just $ simpleCompletion v
                             else Nothing) names 
