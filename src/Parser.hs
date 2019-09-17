{-# LANGUAGE LambdaCase #-}
module Parser where 

import AST
import Parser.Lexer 
import Parser.Function 
import Parser.Types
import Parser.Expression
import Parser.Statement
import Parser.C0ParserState

import Control.Monad.State

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Debug

data Decl = Typedef String C0Type 
          | StructDecl String C0Struct
          | FunctionDecl Function deriving Show

struct = do 
  reserved "struct"
  name <- identifier 
  body <- braces . some $ do 
    VariableDecl fieldName fieldType <- variableDecl 
    semicolon 
    return (fieldName, fieldType)

  lift $ modify (addStruct (name, body))
  return (name, body)

isParsedFunctionDecl = \case 
  FunctionDecl _ -> True 
  _ -> False 

generalDecl = ((uncurry Typedef <$> try typedef) <* semicolon) <|> 
              ((uncurry StructDecl <$> try struct) <* semicolon) <|> 
              (FunctionDecl <$> functionDef)

--replParser :: Parser (Either Expression Statement)
--replParser = (Right <$> try statement) <|> (Left <$> try expression <* option [] semicolon) 
--replParser = try (Left <$> dbg "parser exp" (try (expression <* option [] semicolon))) <|> (Right <$> dbg "parser stmnt" statement) 
replParser :: String -> C0ParserState -> Either String (Either Expression Statement, C0ParserState)
replParser input state = 
  case parse (expression <* option [] semicolon) "(stdin)" input state of 
    Right (e, newState) -> Right (Left e, newState)
    Left err -> case parse statement "(stdin)" input state of 
      Right (s, newState) -> Right (Right s, newState)
      Left err -> case parse statement "(stdin)" (input ++ ";") state of 
        Right (s, newState) -> Right (Right s, newState)
        Left err -> Left err 
