module Parser.Statement where 

import Parser.Lexer
import Parser.AST
import Parser.Types
import Parser.Expression

import Text.Megaparsec
import Text.Megaparsec.Debug 

import Data.Functor
     
statement = (statementBlock <|>
            ifStatement <|>
            whileLoop <|>
            forLoop <|>
            returnStatement <* semicolon <|>
            assertStatement <* semicolon <|>
            errorStatement <* semicolon <|>
            simple <* semicolon)
               
ifStatement = do 
  reserved "if"
  condition <- parens expression
  ifBody <- statement 
  elseBody <- optional $ reserved "else" >> statement 
  
  return $ IfStatement condition ifBody elseBody 

whileLoop = do 
  reserved "while"
  condition <- parens expression
  body <- statement 

  return $ WhileLoop condition body 

forLoop = do 
  reserved "for"
  (init, test, inc) <- parens $ do
    init <- simple 
    semicolon
    test <- expression
    semicolon
    inc <- optional simple
    
    return (init, test, inc)

  body <- statement 

  return $ ForLoop init test inc body 

returnStatement = Return <$> (reserved "return" *> optional expression)
assertStatement = Assert <$> (reserved "assert" *> expression)
errorStatement = Error <$> (reserved "error" *> expression)

statementBlock = StatementBlock <$> (braces $ many statement)

simple = assign <|> inc <|> dec <|> (FunctionCallStmnt <$> expression)
  where assign = do 
          (lhs, op) <- try $ do
                         lhs <- (Right <$> expression) <|> (Left <$> variableDecl); 
                         op <- choice opParsers               
                         return (lhs, op)

          rhs <- expression

          return $ case (lhs, op) of 
                     (_, Nothing) -> Assign lhs rhs 
                     ((Left dec), Just constructor) -> 
                        Assign lhs (BinOp constructor (Identifier $ varName dec) rhs)
                     (Right lhsExp, Just constructor) ->
                        Assign lhs (BinOp constructor lhsExp rhs)

        inc = try $ Increment <$> expression <* symbol "++"
        dec = try $ Decrement <$> expression <* symbol "--"

        mkOp text constructor = symbol text $> Just constructor
        opParsers = [symbol "=" $> Nothing,
                     mkOp "+=" Plus,
                     mkOp "-=" Minus,
                     mkOp "*=" Multiply,
                     mkOp "/=" Divide,
                     mkOp "%=" Mod,
                     mkOp "<<=" LeftShift,
                     mkOp ">>=" RightShift,
                     mkOp "&=" BitAnd,
                     mkOp "^=" Xor,
                     mkOp "|=" BitOr]                          

        
          