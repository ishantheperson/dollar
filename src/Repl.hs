{-# LANGUAGE LambdaCase, TupleSections #-}
module Repl where 

import AST

import Eval
import Eval.Context

import Parser
import Parser.C0ParserState

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Exception

import System.Console.Haskeline hiding (Handler)

repl :: [Function] -> C0ParserState -> IO ((), Context)
repl fs state = runEvalT $ runInputT (settings fs) (loop fs state)

prompt = "\x1b[32;1m$> \x1b[0m"

-- Evaluator is the state of the "main" function that's always running
-- in the interpreter 
loop :: [Function] -> C0ParserState -> InputT Evaluator ()
loop fs state = do 
  getInputLine prompt >>= \case
    Nothing -> do
      outputStrLn "Goodbye"
      return ()
    Just "#quit" -> do 
      outputStrLn "Goodbye"
      return ()
      
    Just "#help" -> do 
      outputStrLn "Use #functions to get a list of all functions"
      outputStrLn "Press TAB to complete code in most cases"
      outputStrLn "Use #quit (or CTRL D) to quit"
      loop fs state
       
    Just "#functions" -> do 
      if null fs 
        then outputStrLn "(no functions declared)"
        else forM_ fs (outputStrLn . showFunctionHeader)
      loop fs state 

    Just input -> do 
      --let parseResult = parse replParser "(input)" input state 
      let parseResult = replParser input state 
      case parseResult :: Either String (Either Expression Statement, C0ParserState) of 
        Left err -> outputStrLn err >> loop fs state 
        Right (result', state') -> do 
          (result, state') <- (lift $ case result' of 
            Left e -> ((,state') <$> evalE fs e) -- `catches` evalExceptionHandlers
            Right stmnt -> ((,state') <$> (C0VoidVal <$ (runExceptT $ evalS fs stmnt)))) -- `catches` evalExceptionHandlers

          outputStrLn =<< liftIO (showC0Value result)
          loop fs state' 

evalExceptionHandlers :: [Handler a]
evalExceptionHandlers = [] 

-- Eventually we will change this to another StateT monad,
-- this one keeping track of functions. Then we can also
-- get TAB completion for new functions instead of just for
-- variables, prexisting functions, and language builtins like we do now
settings :: [Function] -> Settings Evaluator
settings fs = setComplete completer (defaultSettings { historyFile = Just ".dollarHistory" })
  where completer = completeWord Nothing " \t" (findCompletion fs)

        completedReservedWords = ["alloc", "alloc_array"]

        findCompletion :: [Function] -> String -> Evaluator [Completion] 
        findCompletion fs s = do 
          vars <- gets getAllVars 
          let funcCompletions = flip mapMaybe fs $ 
                                  \f -> if s `isPrefixOf` functionName f 
                                          then Just $ Completion (functionName f ++ "(") (showFunctionHeader f) False
                                          else Nothing 

              otherCompletions = flip mapMaybe ((map varName vars) ++ completedReservedWords) $
                                   \w -> if s `isPrefixOf` w 
                                           then Just $ simpleCompletion w 
                                           else Nothing
          return $ funcCompletions ++ otherCompletions
                