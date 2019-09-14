{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Lexer
import Parser.Function

import Control.Monad
import Text.Megaparsec

import System.Environment (getArgs)

main = getArgs >>= \case 
  [] -> getContents >>= (putStrLn . test (many functionDef) "")
  l -> forM_ l (\fileName -> readFile fileName >>= (putStrLn . test (many functionDef) fileName))

  --where parse = parseTest ((many functionDef) <* eof)