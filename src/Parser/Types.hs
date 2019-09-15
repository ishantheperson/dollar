{-# LANGUAGE RecordWildCards #-}
module Parser.Types where

import Parser.Lexer 
import AST

import Text.Megaparsec  
import Text.Megaparsec.Debug

variableDecl = do 
  varType <- parseType
  varName <- identifier

  return VariableDecl{..}

typedef = do 
  reserved "typedef"
  typeValue <- parseType
  typeName <- identifier 
  
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
          C0Int <$ reserved "int" <|>
          C0Char <$ reserved "char" <|>
          C0String <$ reserved "string" <|>
          C0Void <$ reserved "void" <|>
          C0Bool <$ reserved "bool" <|>
          (C0Struct <$ reserved "struct" <*> identifier) <|>
          C0Typedef <$> identifier 