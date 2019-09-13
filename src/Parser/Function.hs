{-# LANGUAGE RecordWildCards #-}
module Parser.Function where 

import Parser.Lexer 
import Parser.Types 
import Parser.Expression 
import Parser.AST

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
--import Text.Megaparsec.Combinator

variableDecl = do 
  varType <- parseType
  varName <- identifier

  return VariableDecl{..}

functionHeader = do 
  functionType <- parseType
  functionName <- identifier 
  functionArgDecls <- parens (variableDecl `sepBy` symbol ",")

  -- TODO: function contracts 
  return Function{..}

functionDecl = functionHeader 