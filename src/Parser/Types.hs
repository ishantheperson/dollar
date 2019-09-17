{-# LANGUAGE RecordWildCards, LambdaCase, ScopedTypeVariables #-}
module Parser.Types where

import AST
import Parser.C0ParserState
import Parser.Lexer 

import Text.Megaparsec  
import Text.Megaparsec.Debug

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

variableDecl :: Parser VariableDecl
variableDecl = do 
  varType <- parseType
  varName <- identifier

  return VariableDecl{..}

typedef = do 
  reserved "typedef"
  typeValue <- parseType
  typeName <- identifier 
  
  --typedefs <- lift $ gets knownTypedefs 
  let theTypedef = (typeName, flattenType typeValue)
  lift $ modify (addTypedef theTypedef)

  --return (typeName, typeValue)
  return theTypedef

flattenType :: C0Type -> C0Type
flattenType = \case 
  C0Typedef _ t -> flattenType t 
  C0Pointer t -> C0Pointer $ flattenType t 
  C0Array t -> C0Array $ flattenType t 
  other -> other 


parseType :: Parser C0Type
parseType = simple >>= postfix
  where postfix t = pointer t <|> array t <|> return t 

        pointer, array :: C0Type -> Parser C0Type
        pointer t = do symbol "*" 
                       postfix $ C0Pointer t

        array t = do symbol "["
                     symbol "]" 
                     postfix $ C0Array t

        typedefT :: Parser C0Type
        typedefT = do 
          s <- identifier
          maybeT <- lift $ gets (lookupTypedef s)
          case maybeT of 
            Just t -> return $ C0Typedef s t 
            Nothing -> fail $ "unknown typedef name '" ++ s ++ "'" 

        simple = 
          C0Int <$ reserved "int" <|>
          C0Char <$ reserved "char" <|>
          C0String <$ reserved "string" <|>
          C0Void <$ reserved "void" <|>
          C0Bool <$ reserved "bool" <|>
          (C0Struct <$ reserved "struct" <*> identifier) <|>
          typedefT  