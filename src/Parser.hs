module Parser where 

import Parser.Lexer 
import Parser.Expression 

import Text.Megaparsec
import Text.Megaparsec.Char

data Decl = Typedef String C0Type 
          | FunctionDecl Function

{-
contract = lineContract <|> blockContract 
  where lineContract = do 
          lexeme $ string "//@"
          a <- expression 
          semicolon
      
          return a
-}

generalDecl = (uncurry Typedef <$> typedef) <|> (Function <$> function) <* semicolon
  