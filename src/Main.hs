{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
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

data Flag = ShowIntBase Int | EnableContracts deriving (Show, Eq)

options = [ Option ['x'] ["hex"] (NoArg $ ShowIntBase 16) "prints out integers in hex",
            Option ['c', 'd'] ["dyn-check", "check-contracts"] (NoArg EnableContracts) "enables runtime contract checks" ]

main :: IO ()
main = do 
  putStr "\x1b[32;1m" -- green bold
  putStrLn "Welcome to Dollar, version -1"
  putStr "\x1b[0m" -- reset 

  putStrLn "For help, type #help"

  --getArgs >>= getOpt Permute options & \case 
  getArgs >>= \case 
    [] -> () <$ repl builtinFunctions defaultState
    
    files-> do 
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
  where parseDecls = parse (many generalDecl) 

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