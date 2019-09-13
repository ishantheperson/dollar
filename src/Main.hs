{-# LANGUAGE LambdaCase #-}
module Main where 

import Parser.Function
import Text.Megaparsec

import Control.Monad

import System.Environment (getArgs)

main = getArgs >>= \case 
  [] -> getContents >>= parse 
  l -> forM_ l (readFile >=> parse)
  
  where parse = parseTest ((many functionDef) <* eof)