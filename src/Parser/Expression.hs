module Parser.Expression where

import Parser.Lexer 
import Parser.AST
import Parser.Types

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
--import Text.Megaparsec.Debug

expression :: Parser Expression
expression = makeExprParser (term >>= postfix) operators <?> "expression"
  where postfix e = dotAccess e <|> 
                    arrayAccess e <|> 
                    arrowAccess e <|> 
                    functionCall e <|>
                    return e 

        dotAccess e = do symbol "."
                         fieldName <- identifier 
                         postfix $ StructDotAccess e fieldName 

        arrowAccess e = do symbol "->"
                           fieldName <- identifier 
                           postfix $ StructArrowAccess e fieldName

        arrayAccess e = do index <- squareBrackets expression 
                           postfix $ ArrayAccess e index                        

        functionCall e = parens $ (FunctionCall e) <$> expression `sepBy` symbol ","
                                     
term :: Parser Expression 
term = parens expression <|> 
       allocArray <|> alloc <|> 
       IntConstant <$> integer <|> 
       StringLiteral <$> stringLiteral <|> 
       CharLiteral <$> charLiteral <|>
       BoolLiteral True <$ symbol "true" <|>
       BoolLiteral False <$ symbol "false" <|>
       Identifier <$> identifier 
  where alloc = Alloc <$> (reserved "alloc" *> parens parseType)
        allocArray = do reserved "alloc_array"
                        parens (AllocArray <$> (parseType <* symbol ",") <*> expression)                         

operators :: [[Operator Parser Expression]]
operators = [[prefixOp Negate "-",
              prefixOp BitNot "~",
              prefixOp BoolNot "!",
              prefixOp PointerDeref "*"],

             [binOp' Multiply "*",
              binOp' Divide "/",
              binOp' Mod "%"],

              -- Prevents problems with a++ or a +=
             [InfixL ((BinOp Plus) <$ (try $ symbol "+" <* notFollowedBy (oneOf "+="))),
              InfixL ((BinOp Minus) <$ (try $ symbol "-" <* notFollowedBy (oneOf "-=")))],

             [binOp' LeftShift "<<",
              binOp' RightShift ">>"],

             [binOp'' Less "<",
              binOp'' LessEqual "<=",
              binOp'' Greater ">",
              binOp'' GreaterEqual ">="],

             [binOp Equal "==",
              binOp NotEqual "!="],

             [binOp' BitAnd "&"],
             [binOp' Xor "^"],
             [binOp' BitOr "|"],
             
             [TernR ((Ternary <$ symbol ":") <$ symbol "?")]
             ]
  where prefixOp constructor sym = Prefix (UnaryOp constructor <$ symbol sym)
        binOp constructor sym = InfixL ((BinOp constructor) <$ symbol sym)
        -- This is for operators which have a compound assignment counterpart
        -- This way the + in += doesn't get lexed individually, for example.
        binOp' constructor sym = InfixL ((BinOp constructor) <$ (try $ symbol sym <* notFollowedBy (char '=')))
        -- This stops x >>= as being parsed as x > ..  
        binOp'' constructor sym = InfixL ((BinOp constructor) <$ (try $ symbol sym <* notFollowedBy (string sym <|> string "=")))