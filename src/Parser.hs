module Parser where 

import Parser.Lexer 
import Parser.Expression 
import Parser.Function 
import Parser.Types
import Parser.AST

import Text.Megaparsec
import Text.Megaparsec.Char

data Decl = Typedef String C0Type 
          | FunctionDecl Function deriving Show

generalDecl = (try $ (uncurry Typedef <$> typedef) <* semicolon) <|> 
              (FunctionDecl <$> functionDef)
  