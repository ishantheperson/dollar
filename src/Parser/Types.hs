module Parser.Types where

import Parser.Lexer 

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Functor

data C0Type = C0Int 
            | C0Char
            | C0String
            | C0Bool
            | C0Void
            | C0Typedef String 
            | C0Pointer C0Type
            | C0Array C0Type deriving Show 
            -- also need function ptr types

simple = 
  reserved "int" $> C0Int<|>
  reserved "char" $> C0Char <|>
  reserved "string" $> C0String<|>
  reserved "void" $> C0Void <|>
  reserved "bool" $> C0Bool <|>
  C0Typedef <$> identifier 

parseType :: Parser C0Type
parseType = simple >>= postfix
  where postfix t = pointer t <|> array t <|> return t 

        pointer, array :: C0Type -> Parser C0Type
        pointer t = do symbol "*" 
                       postfix $ C0Pointer t
        array t = do symbol "["
                     symbol "]" 
                     postfix $ C0Array t