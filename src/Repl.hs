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

repl :: [Function] -> IO ((), Context)
repl fs = runEvalT $ runInputT (settings fs) (loop fs)

prompt = "\x1b[32;1m$> \x1b[0m"

loop :: [Function] -> InputT Evaluator ()
loop fs = do 
  getInputLine prompt >>= \case
    Nothing -> return ()
    Just "#help" -> do 
      outputStrLn "Use #functions to get a list of all functions"
      outputStrLn "Press TAB to complete code in most cases"
      outputStrLn "Use #quit (or CTRL D) to quit"
      loop fs 
    Just "#quit" -> return ()
    Just "#functions" -> do 
      if null fs 
        then outputStrLn "(no functions declared)"
        else forM_ fs (outputStrLn . functionName)
      loop fs 

    Just input -> do 
      let parseResult = test' replParser input 
      result <- lift $ case parseResult of 
                  Left e -> evalE fs e 
                  Right stmnt -> C0VoidVal <$ (runExceptT $ evalS fs stmnt)

      outputStrLn =<< liftIO (showC0Value result)
      loop fs

-- Eventually we will change this to another StateT monad,
-- this one keeping track of functions. Then we can also
-- get TAB completion for new functions instead of just for
-- variables, prexisting functions, and language builtins like we do now
settings :: [Function] -> Settings (Evaluator)
settings fs = setComplete completer (defaultSettings { historyFile = Just ".dollarHistory" })
  where completer = completeWord Nothing " \t" (findCompletion fs)

        findCompletion :: [Function] -> String -> Evaluator [Completion] 
        findCompletion fs s = do 
          vars <- gets getAllVars
          let funcNames = map functionName fs 
              varNames = map varName vars 
        
              names = funcNames ++ varNames ++ reservedWords
          return $ mapMaybe (\v -> if s `isPrefixOf` v  
                                     then Just $ simpleCompletion v
                                     else Nothing) names 
