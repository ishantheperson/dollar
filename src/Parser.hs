module Parser where 

import Parser.Lexer 
import Parser.Expression 
import Parser.Function 
import Parser.Types

import Text.Megaparsec
import Text.Megaparsec.Char

data Decl = Typedef String C0Type 
          | FunctionDecl Function

generalDecl = (uncurry Typedef <$> typedef) <|> (Function <$> function) <* semicolon
  