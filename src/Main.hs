{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Lexer
import Parser.Function
import Text.Megaparsec

import Control.Monad

import System.Environment (getArgs)

main = getArgs >>= \case 
  [] -> getContents >>= (print . test functionDef "" )
  l -> forM_ l (\fileName -> readFile fileName >>= (print . test functionDef fileName))

  --where parse = parseTest ((many functionDef) <* eof)