module Parser where 

import Parser.Lexer 
import Parser.Function 
import Parser.Types
import AST

import Text.Megaparsec

data Decl = Typedef String C0Type 
          | FunctionDecl Function deriving Show

generalDecl = (try $ (uncurry Typedef <$> typedef) <* semicolon) <|> 
              (FunctionDecl <$> functionDef)
  