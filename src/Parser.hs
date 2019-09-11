module Parser where 

import Parser.Lexer 
import Parser.Expression 

import Text.Megaparsec
import Text.Megaparsec.Char

contract = lineContract <|> blockContract 
  where lineContract = do 
          lexeme $ string "//@"
          a <- expression 
          semicolon
      
          return a