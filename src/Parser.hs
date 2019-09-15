{-# LANGUAGE LambdaCase #-}
module Parser where 

import Parser.Lexer 
import Parser.Function 
import Parser.Types
import Parser.Expression
import Parser.Statement
import AST

import Text.Megaparsec

data Decl = Typedef String C0Type 
          | FunctionDecl Function deriving Show

isParsedFunctionDecl = \case 
  FunctionDecl _ -> True 
  _ -> False 

generalDecl = (try $ (uncurry Typedef <$> typedef) <* semicolon) <|> 
              (FunctionDecl <$> functionDef)

replParser :: Parser (Either Expression Statement)
replParser = (Right <$> try statement) <|> (Left <$> try expression <* option [] semicolon) 