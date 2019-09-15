{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser
import Parser.Lexer

import Repl

import Text.Megaparsec (many)

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Flag = ShowIntBase Int | EnableContracts deriving (Show, Eq)

options = [ Option ['x'] ["hex"] (NoArg $ ShowIntBase 16) "prints out integers in hex",
            Option ['c', 'd'] ["dyn-check", "check-contracts"] (NoArg EnableContracts) "enables runtime contract checks" ]

main :: IO ()
main = do 
  putStr "\x1b[32;1m"
  putStrLn "Welcome to Dollar, version -1"
  putStr "\x1b[0m"

  putStrLn "For help, type #help"

  getArgs >>= \case 
    [] -> return () 
      
    file:_ -> do 
      fileContents <- readFile file 
      decls <- case parseDecls file fileContents of 
                 Left err -> do putStrLn err 
                                exitFailure
                 Right v -> return v 
  
      let fs = filter isParsedFunctionDecl decls 
  
  
      repl (map (\(FunctionDecl f) -> f) fs)
      return () 
    
  where parseDecls = parse (many generalDecl) 

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