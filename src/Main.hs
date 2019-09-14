{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Lexer
import Parser.Function
import Parser

import Repl

import Control.Monad
import Text.Megaparsec (many)

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Flag = ShowIntBase Int | EnableContracts deriving (Show, Eq)

options = [ Option ['x'] ["hex"] (NoArg $ ShowIntBase 16) "prints out integers in hex" ]

main :: IO ()
main = getArgs >>= \case 
  [] -> do 
    putStrLn "Dollar version -1"
    
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