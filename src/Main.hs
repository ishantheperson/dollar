{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Lexer
import Parser.Function
import Parser

import Control.Monad
import Text.Megaparsec

import System.Environment (getArgs)

main = getArgs >>= \case 
  [] -> getContents >>= (putStrLn . test (sc >> many generalDecl) "")
  l -> forM_ l (\fileName -> readFile fileName >>= (putStrLn . test (sc >> many generalDecl) fileName))

  --where parse = parseTest ((many functionDef) <* eof)