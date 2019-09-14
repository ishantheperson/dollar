{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Lexer
import Parser.Function
import Parser

import Repl

import Control.Monad
import Text.Megaparsec

import System.Environment (getArgs)

getContents' = getContents >>= \s -> length s `seq` return s

--main :: IO ()
main = getArgs >>= \case 
  [] -> do 
    putStrLn "Reading from stdin..."
    stdin <- getContents' 
    let decls = parse stdin 
        fs = filter isParsedFunctionDecl decls 

    repl (map (\(FunctionDecl f) -> f) fs)
  
  file:_ -> do 
    fileContents <- readFile file 
    let decls = parse fileContents 
        fs = filter isParsedFunctionDecl decls 

    repl (map (\(FunctionDecl f) -> f) fs)
{-
  files -> do 
    decls <- concat <$> forM files $ \file -> do 
                          fileContents <- readFile file 
                          let decls = parse fileContents 
                          return $ filter isParsedFunctionDecl decls 

    repl (map (\(FunctionDecl f) -> f) decls)
-}
  where parse = test (many generalDecl) "(stdin)"

{-
main = getArgs >>= \case 
  [] -> getContents >>= (putStrLn . test (sc >> many generalDecl) "")
  l -> forM_ l (\fileName -> readFile fileName >>= (putStrLn . test (sc >> many generalDecl) fileName))
-}
  --where parse = parseTest ((many functionDef) <* eof)