{-# LANGUAGE LambdaCase #-}
module Parser.Lexer where  

import Parser.C0ParserState

import Control.Monad (void)
import Control.Monad.State.Strict
--import Control.Monad.Writer.Strict -- TODO: for error recovery 

import Data.Void 
import Data.Int

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex 

type Parser = ParsecT Void String (State C0ParserState)
--type Parser = Parsec Void String 

test :: Show a => Parser a -> FilePath -> String -> a
test p fileName input = 
  case evalState (runParserT (sc >> (p <* eof)) fileName input) defaultState of 
    Left err -> error $ errorBundlePretty err 
    Right v ->  v

-- | Skips leading whitespace, then calls parser p on input until EOF
--   Crashes and prints the error when it encounters one, otherwise 
--   returns the parsed value 
test' p input = 
  case evalState (runParserT (sc >> (p <* eof)) "" input) defaultState of 
    Left err -> error $ errorBundlePretty err 
    Right v -> v 

reservedWords = [ "if",
                  "else",
                  "while",
                  "break",
                  "continue",
                  "for",

                  "return",

                  "int",
                  "char",
                  "string",
                  "bool",
                  "void",

                  "true",
                  "false",

                  "typedef",
                  "struct",
                  "alloc",
                  "alloc_array",

                  "assert"
                ]

-- | Space consumer 
sc, scRegular, scLineContract, scBlockContract :: Parser () 
sc = lift (gets parsingMode) >>= \case Regular -> scRegular
                                       LineContract -> scLineContract
                                       BlockContract -> scBlockContract

scRegular = Lex.space space1 lineComment blockComment 
-- In contracts, @ is a space char (not specified in the language spec tho I think)
-- In line contracts, don't allow newlines 
scLineContract = Lex.space (skipSome (oneOf "@ \t")) lineComment blockComment
scBlockContract = Lex.space (skipSome (void spaceChar <|> (try (char '@' >> notFollowedBy (string "*"))))) lineComment blockComment

lineComment = do 
  try $ string "//" >> notFollowedBy (char '@')
  void $ manyTill anySingle (char '\n')
blockComment = do 
  try $ string "/*" >> notFollowedBy (char '@')
  void $ manyTill anySingle (string "*/")


lexeme :: Parser a -> Parser a 
lexeme = Lex.lexeme sc

parens, squareBrackets, braces :: Parser a -> Parser a 
parens = between (symbol "(") (symbol ")")
squareBrackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

symbol :: String -> Parser String 
symbol = Lex.symbol sc

charLiteral = lexeme $ char '\'' >> (Lex.charLiteral <* char '\'')
stringLiteral = lexeme $ char '"' >> manyTill Lex.charLiteral (char '"')

-- | Parses an integer value and checks to make sure 
integer :: Parser Integer
integer = lexeme (try $ char '0' >> char' 'x' >> Lex.hexadecimal) <|> lexeme Lex.decimal
-- We don't want to check this here, otherwise we have to deal with special cases for -0x80000000 etc 
{-             if toInteger (minBound :: Int32) <= num && num <= toInteger (maxBound :: Int32) 
               then return (fromInteger num) 
               else fail $ "integer constant " ++ show num ++ " out of bounds" -}

semicolon = symbol ";"
reserved word = (lexeme . try) (string word *> notFollowedBy alphaNumChar)


identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where p = (:) <$> identStart <*> many identLetter
        identStart = letterChar
        identLetter = alphaNumChar <|> char '_'

        check x = if x `elem` reservedWords 
                    then fail $ "'" ++ x ++ "' cannot be an identifier"
                    else return x 