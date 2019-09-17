module Parser.Contract where 

import AST 
import Parser.Lexer
import Parser.Expression 
import Parser.C0ParserState

import Control.Monad.State
import Control.Monad.Combinators (choice)

import Text.Megaparsec

contractBlock :: Parser [Contract]
contractBlock = concat <$> many (lineContract <|> blockContract)
nonemptyContractBlock = concat <$> some (lineContract <|> blockContract)

lineContract = do 
  symbol "//@"

  oldState <- get
  lift $ put (setParserMode LineContract oldState)

  contracts <- contract `endBy1` semicolon 

  lift $ put oldState
  sc -- not sure if necessary
  
  return contracts 

blockContract = do 
  symbol "/*@"
  oldState <- get
  lift $ put (setParserMode BlockContract oldState)

  contracts <- contract `endBy1` semicolon

  symbol "@*/"
  lift $ put oldState
  sc 

  return contracts 

contract = do 
  contractType <- choice contractTypes
  contractExpression <- expression 

  return $ Contract contractType contractExpression

  where contractTypes = [Requires <$ symbol "requires",
                         Ensures <$ symbol "ensures",
                         LoopInvariant <$ symbol "loop_invariant",
                         AssertContract <$ symbol "assert"]

