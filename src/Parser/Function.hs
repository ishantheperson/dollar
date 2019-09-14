{-# LANGUAGE RecordWildCards #-}
module Parser.Function where 

import Parser.Lexer 
import Parser.Statement 
import Parser.Types 
import Parser.AST
import Parser.Contract

import Text.Megaparsec
--import Text.Megaparsec.Debug
--import Text.Megaparsec.Combinator

functionHeader = do 
  functionType <- parseType
  functionName <- identifier 
  functionArgDecls <- parens (variableDecl `sepBy` symbol ",")

  -- TODO: function contracts 
  return (functionType, functionName, functionArgDecls)

functionDef = do 
  (functionType, functionName, functionArgDecls) <- functionHeader 
  functionContracts <- contractBlock
  functionBody <- braces (many statement) 
  return Function{..}