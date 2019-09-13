{-# LANGUAGE RecordWildCards #-}
module Parser.Types where

import Parser.Lexer 
import Parser.AST

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

import Data.Functor

variableDecl = do 
  varType <- parseType
  varName <- identifier

  return VariableDecl{..}

typedef = do 
  reserved "typedef"
  typeName <- identifier 
  typeValue <- parseType
  
  --semicolon

  return (typeName, typeValue)

parseType :: Parser C0Type
parseType = simple >>= postfix
  where postfix t = pointer t <|> array t <|> return t 

        pointer, array :: C0Type -> Parser C0Type
        pointer t = do symbol "*" 
                       postfix $ C0Pointer t

        array t = do symbol "["
                     symbol "]" 
                     postfix $ C0Array t

        simple = 
          reserved "int" $> C0Int<|>
          reserved "char" $> C0Char <|>
          reserved "string" $> C0String<|>
          reserved "void" $> C0Void <|>
          reserved "bool" $> C0Bool <|>
          C0Typedef <$> identifier 