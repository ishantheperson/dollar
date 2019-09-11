module Parser where 

import Parser.Lexer 
import Parser.Expression 

import Text.Megaparsec
import Text.Megaparsec.Char

contract = do 
  lexeme $ string "//@"
  a <- identifier
  semicolon

  return a