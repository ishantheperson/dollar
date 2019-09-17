{-# LANGUAGE LambdaCase, ScopedTypeVariables, MultiWayIf #-}
module Main where 

import Parser
import Parser.Lexer
import Parser.C0ParserState

import Eval.Builtin

import Repl

import Data.Function ((&))
import Text.Megaparsec (many)

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Arrow ((&&&))

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Flag = ShowIntBase Int | EnableContracts | Help deriving (Show, Eq)

options = [ Option ['x'] ["hex"] (NoArg $ ShowIntBase 16) "prints out integers in hex",
            Option [] ["binary"] (NoArg $ ShowIntBase 2) "prints out integers in ",
            Option [] ["base"] (ReqArg (ShowIntBase . readBase) "BASE") "selects base to print integers",
            Option ['c', 'd'] ["dyn-check", "check-contracts"] (NoArg EnableContracts) "enables runtime contract checks",
            Option ['h'] ["help"] (NoArg Help) "show this help message"]

  where readBase :: String -> Int 
        readBase s = 
          let num = read s in 
          if num < 2 || num > 16 
            then error "Base must be between 2 and 16" -- FIXME: Validate options before launch REPL properly
            else num 
            
findBase :: [Flag] -> Int 
findBase = \case [] -> 10
                 (ShowIntBase i):_ -> i 
                 _:xs -> findBase xs 

main :: IO ()
main = do 
  putStr "\x1b[32;1m" -- green bold
  putStrLn "Welcome to Dollar, version -1"
  putStr "\x1b[0m" -- reset 

  putStrLn "For help, type #help"

  o <- getOpt Permute options <$> getArgs
  case o of 
    (opts, files, []) -> do
      if | Help `elem` opts -> printHelp "Dollar C0 interpreter"
         | otherwise -> do 
          let base = findBase opts 
              replOpts = ReplFlags { replIntBase = base }
          if null files 
            then () <$ repl builtinFunctions defaultState replOpts
            else do 
              fileContents <- concat <$> mapM readFile files 
              (decls, state) <- case parseDecls "" fileContents defaultState of 
                                  Left err -> do putStrLn err 
                                                 exitFailure
                                  Right v -> return v 
          
              let fs = filter isParsedFunctionDecl decls 
          
              --print =<< snd <$> repl (builtinFunctions ++ (map (\(FunctionDecl f) -> f) fs)) state 
              repl (builtinFunctions ++ (map (\(FunctionDecl f) -> f) fs)) state replOpts
              return ()         
    (_, _, errors) -> printHelp $ concat errors 

{-
  getArgs >>= \case 
    [] -> () <$ repl builtinFunctions defaultState
    
    files -> do 
      --putStrLn $ "Now loading '" ++ file ++ "'..."
      fileContents <- concat <$> mapM readFile files 
      (decls, state) <- case parseDecls "" fileContents defaultState of 
                          Left err -> do putStrLn err 
                                         exitFailure
                          Right v -> return v 
  
      let fs = filter isParsedFunctionDecl decls 
  
      --print =<< snd <$> repl (builtinFunctions ++ (map (\(FunctionDecl f) -> f) fs)) state 
      repl (builtinFunctions ++ (map (\(FunctionDecl f) -> f) fs)) state 
      return () 
-}
  where parseDecls = parse (many generalDecl) 
        printHelp e = putStrLn e >> putStr (usageInfo "usage: dollar (flags) (files) - Ishan" options ) 

--parseC0File :: FilePath -> StateT C0ParserState IO 

{-
  files -> do 
    decls <- concat <$> forM files $ \file -> do 
                          fileContents <- readFile file 
                          let decls = parse fileContents 
                          return $ filter isParsedFunctionDecl decls 

    repl (map (\(FunctionDecl f) -> f) decls)
-}

{-
main = getArgs >>= \case 
  [] -> getContents >>= (putStrLn . test (sc >> many generalDecl) "")
  l -> forM_ l (\fileName -> readFile fileName >>= (putStrLn . test (sc >> many generalDecl) fileName))
-}
  --where parse = parseTest ((many functionDef) <* eof)