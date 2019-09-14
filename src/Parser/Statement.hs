module Parser.Statement where 

import Parser.Lexer
import Parser.AST
import Parser.Types
import Parser.Expression
import Parser.Contract

import Text.Megaparsec
import Text.Megaparsec.Debug 

import Data.Functor
     
statement = statementBlock <|>
            ifStatement <|>
            whileLoop <|>
            forLoop <|>
            returnStatement <* semicolon <|>
            assertStatement <* semicolon <|>
            errorStatement <* semicolon <|>
            (StatementContract <$> nonemptyContractBlock) <|>
            simple <* semicolon
               
ifStatement = do 
  reserved "if"
  condition <- parens expression
  ifBody <- statement 
  elseBody <- optional $ reserved "else" >> statement 
  
  return $ IfStatement condition ifBody elseBody 

whileLoop = do 
  reserved "while"
  condition <- parens expression
  contracts <- contractBlock
  body <- statement 

  return $ WhileLoop condition contracts body 

forLoop = do 
  reserved "for"
  (init, test, inc) <- parens $ do
    init <- simple 
    semicolon
    test <- expression
    semicolon
    inc <- optional simple
    
    return (init, test, inc)

  contracts <- contractBlock

  body <- statement 

  return $ ForLoop init test inc contracts body 

returnStatement = Return <$> (reserved "return" *> optional expression)
assertStatement = Assert <$> (reserved "assert" *> expression)
errorStatement = Error <$> (reserved "error" *> expression)

statementBlock = StatementBlock <$> (braces $ many statement)

simple = assign <|> inc <|> dec -- <|> (FunctionCallStmnt <$> expression)
  where assign = try $ do 
          (lhs, op) <- try $ do
                         lhs <- (try (Left <$> variableDecl)) <|> (try (Right <$> expression)); 
                         op <-  (try (Just <$> choice opParsers)) <|> return Nothing 
                         return (lhs, op)

          case (lhs, op) of 
            (Left d, Nothing) -> return $ VariableDeclStmnt d 
            (Left d, Just op') -> do 
              rhs <- expression 
              return $ case op' of 
                Nothing -> DeclAssign d rhs 
                Just compoundOp -> DeclAssign d (BinOp compoundOp (Identifier $ varName d) rhs)

            (Right _, Nothing) -> fail "" -- This case is handled by inc/dec, but we could change it 
            (Right lhs, Just op') -> do 
              rhs <- expression
              return $ case op' of 
                Nothing -> Assign lhs rhs 
                Just compoundOp -> Assign lhs (BinOp compoundOp lhs rhs)

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

        
          