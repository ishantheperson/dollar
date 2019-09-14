{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Lexer
import Parser.Function

import Control.Monad

import System.Environment (getArgs)

main = getArgs >>= \case 
  [] -> getContents >>= (putStrLn . test functionDef "")
  l -> forM_ l (\fileName -> readFile fileName >>= (putStrLn . test functionDef fileName))

  --where parse = parseTest ((many functionDef) <* eof)