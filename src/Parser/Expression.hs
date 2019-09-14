{-# LANGUAGE FlexibleContexts #-}
module Parser.Expression where

import Parser.Lexer 
import AST
import Parser.Types
import Parser.C0ParserState

import Control.Monad
import Control.Monad.State
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
       contractLength <|> hasTag <|> contractResult <|>
       IntConstant <$> integer <|> 
       StringLiteral <$> stringLiteral <|> 
       CharLiteral <$> charLiteral <|>
       BoolLiteral True <$ symbol "true" <|>
       BoolLiteral False <$ symbol "false" <|>
       NullConst <$ symbol "NULL" <|>
       Identifier <$> identifier 

  where alloc = Alloc <$> (reserved "alloc" *> parens parseType)
        allocArray = do reserved "alloc_array"
                        parens (AllocArray <$> (parseType <* symbol ",") <*> expression)   
                        
        contractLength = do guardContract "\\length only allowed in contracts" -- experimental
                            reserved "\\length" *> parens (ContractLength <$> expression)
        hasTag = do reserved "\\hastag" 
                    parens (HasTag <$> (parseType <* symbol ",") <*> expression)
        contractResult = ContractResult <$ reserved "\\result" 
        -- might remove this since it's not something
        -- we should check in the parser 
        guardContract msg = do allowed <- lift get >>= return . isParsingContract . parsingMode
                               when (not allowed) (fail msg)

operators :: [[Operator Parser Expression]]
operators = [[prefixOp Negate "-",
              prefixOp BitNot "~",
              prefixOp BoolNot "!",
              prefixOp PointerDeref "*"],

             [binOp' (ArithOp Multiply) "*",
              binOp' (ArithOp Divide) "/",
              binOp' (ArithOp Mod) "%"],

              -- Prevents problems with a++ or a +=
             [InfixL ((BinOp $ ArithOp Plus) <$ (try $ symbol "+" <* notFollowedBy (oneOf "+="))),
              InfixL ((BinOp $ ArithOp Minus) <$ (try $ symbol "-" <* notFollowedBy (oneOf "-=")))],

             [binOp' (ArithOp LeftShift) "<<",
              binOp' (ArithOp RightShift) ">>"],

             [binOp'' (CmpOp Less) "<",
              binOp'' (CmpOp LessEqual) "<=",
              binOp'' (CmpOp Greater) ">",
              binOp'' (CmpOp GreaterEqual) ">="],

             [binOp (CmpOp Equal) "==",
              binOp (CmpOp NotEqual) "!="],

             [binOp'' (ArithOp BitAnd) "&"],
             [binOp' (ArithOp Xor) "^"],
             [binOp'' (ArithOp BitOr) "|"],

             [binOp (BoolOp BoolAnd) "&&"],
             [binOp (BoolOp BoolOr) "||"],
             
             [TernR ((Ternary <$ symbol ":") <$ symbol "?")]
             ]
  where prefixOp constructor sym = Prefix (UnaryOp constructor <$ symbol sym)
        binOp constructor sym = InfixL ((BinOp constructor) <$ symbol sym)
        -- This is for operators which have a compound assignment counterpart
        -- This way the + in += doesn't get lexed individually, for example.
        binOp' constructor sym = InfixL ((BinOp constructor) <$ (try $ symbol sym <* notFollowedBy (char '=')))
        -- This stops x >>= as being parsed as x > ..  
        binOp'' constructor sym = InfixL ((BinOp constructor) <$ (try $ symbol sym <* notFollowedBy (string sym <|> string "=")))