{-# LANGUAGE LambdaCase, TupleSections, NPlusKPatterns, ScopedTypeVariables #-}
module Repl where 

import AST

import Eval
import Eval.Context

import Parser
import Parser.C0ParserState

import Data.Int
import Data.Char 
import Numeric 
import Data.Array.MArray
import qualified Data.Map as Map
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (mapMaybe)

import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Exception

import System.Console.Haskeline hiding (Handler)

type ReplT = ReaderT ReplFlags Evaluator

data ReplFlags = ReplFlags {
  replIntBase :: Int
} deriving Show 

-- Because 
showC0Value :: ReplFlags -> C0Value -> IO String
showC0Value flags = \case 
  --C0IntVal i -> return $ show i -- TODO: number formats via Reader monad 
  C0IntVal i -> 
    let base = replIntBase flags 
        num = if i < 0 then i + (maxBound :: Int32) else i 
    in return $ case base of 
         10 -> show i 
         16 -> "0x" ++ zeroPad 8 (showHex num "")
         2 -> "0b" ++ zeroPad 32 (showIntAtBase 2 intToDigit num "")
         _ -> showIntAtBase base intToDigit (fromIntegral num) "" 

  C0StringVal s -> return $ show s 
  C0CharVal c -> return $ show c
  C0BoolVal True -> return "true"
  C0BoolVal False -> return "false"
  C0VoidVal -> return "(void)"
  C0PointerVal _ Nothing -> return "NULL"
  C0PointerVal t _ -> return $ "<" ++ show t ++ "*>"
  C0StructVal s -> do 
    fieldStrings :: [String] <- forM (Map.toList s) $ \(fieldName, fieldVal) -> do
      shownVal <- showC0Value flags fieldVal 
      return $ fieldName ++ "=" ++ shownVal

    return $  "{" ++ intercalate ", " fieldStrings ++ "}"

  C0ArrayVal _ a -> do arrayElems <- getElems a
                       strings <- mapM (showC0Value flags) arrayElems
                       return $ "{" ++ intercalate ", " strings ++ "}"
  where -- zeroPad :: IString -> String 
        zeroPad len s = let difference = len - length s
                        in go s difference
          where go s = \case 
                  0 -> s 
                  n + 1 -> '0':go s n  


repl :: [Function] -> C0ParserState -> ReplFlags -> IO ((), Context)
repl fs state flags = runEvalT $ flip runReaderT flags $ runInputT (settings fs) (loop fs state)

prompt = "\x1b[32;1m$> \x1b[0m"

-- Evaluator is the state of the "main" function that's always running
-- in the interpreter 
loop :: [Function] -> C0ParserState -> InputT ReplT ()
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

    Just "" -> loop fs state 
    Just input -> do 
      --let parseResult = parse replParser "(input)" input state 
      let parseResult = replParser input state 
      -- Yikes this got messy sorry 
      case parseResult :: Either String (Either Expression Statement, C0ParserState) of 
        Left err -> outputStrLn err >> loop fs state 
        Right (result', state') -> do 
          (result, state') <- (lift . lift $ case result' of 
            Left e -> ((,state') <$> evalE fs e) -- `catches` evalExceptionHandlers
            Right stmnt -> ((,state') <$> (C0VoidVal <$ (runExceptT $ evalS fs stmnt)))) -- `catches` evalExceptionHandlers

          outputStrLn =<< liftIO . flip showC0Value result =<< lift ask
          loop fs state' 

evalExceptionHandlers :: [Handler a]
evalExceptionHandlers = [] 

-- Eventually we will change this to another StateT monad,
-- this one keeping track of functions. Then we can also
-- get TAB completion for new functions instead of just for
-- variables, prexisting functions, and language builtins like we do now
settings :: [Function] -> Settings ReplT
settings fs = setComplete completer (defaultSettings { historyFile = Just ".dollarHistory" })
  where completer = completeWord Nothing " \t" (findCompletion fs)

        completedReservedWords = ["alloc", "alloc_array"]

        findCompletion :: [Function] -> String -> ReplT [Completion] 
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
                