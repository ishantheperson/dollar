module Parser.Statement where 

import Parser.Lexer
import Parser.AST
import Parser.Types
import Parser.Expression
import Parser.Function 

import Text.Megaparsec
import Text.Megaparsec.Debug 

import Data.Functor

lvalue = expression -- We check if its valid later 
  
data Statement = Assign (Either VariableDecl Expression) Expression
               | Increment Expression
               | Decrement Expression
               | LoneExpression Expression deriving Show -- e.g. function call
     

simple = assign <|> inc <|> dec <|> (LoneExpression <$> expression)
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

        
          