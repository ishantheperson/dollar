module Parser.Expression where

import Parser.Lexer 

import Data.Int 

import Data.Functor 

import Control.Monad.Combinators.Expr
import Text.Megaparsec

data Expression = -- Terms
                  IntConstant Int32 
                | StringLiteral String 
                | CharLiteral Char 
                | Identifier String

                | FunctionCall String [Expression]
                | Ternary Expression Expression Expression

                | ArrayAccess Expression Expression

                | StructDotAccess Expression String 
                | StructArrowAccess Expression String 
                
                  -- Expression 
                | BinOp BinOperator Expression Expression
                | UnaryOp UnaryOperator Expression
                    deriving Show
-- Note that ++ and -- are missing because they are actually
-- not expressions, instead they can only be used as statements in C0

data BinOperator = Plus | Minus | Multiply | Mod | Divide 
                 | BitAnd | BitOr | Xor | LeftShift | RightShift 
                 | BoolAnd | BoolOr
                 | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual deriving Show
data UnaryOperator =
    -- | Integer negation e.g. -1
    Negate
    -- | Bitwise negation e.g. ~0x33
  | BitNot 
  | BoolNot
  | PointerDeref
  deriving Show

expression = makeExprParser (term >>= postfix) operators <?> "expression"
  where postfix e = dotAccess e <|> 
                    arrayAccess e <|> 
                    arrowAccess e <|> 
                    return e <|>
                    functionCall e 

        dotAccess e = do symbol "."
                         fieldName <- identifier 
                         postfix $ StructDotAccess e fieldName 

        arrowAccess e = do symbol "->"
                           fieldName <- identifier 
                           postfix $ StructArrowAccess e fieldName

        arrayAccess e = do index <- squareBrackets expression 
                           postfix $ ArrayAccess e index 

        functionCall _ = fail "not implemented"

term :: Parser Expression 
term = parens term <|> 
       IntConstant <$> integer <|> 
       StringLiteral <$> stringLiteral <|> 
       CharLiteral <$> charLiteral <|>
       Identifier <$> identifier 

operators :: [[Operator Parser Expression]]
operators = [[prefixOp Negate "-",
              prefixOp BitNot "~",
              prefixOp BoolNot "!",
              prefixOp PointerDeref "*"],

             [binOp Multiply "*",
              binOp Divide "/",
              binOp Mod "%"],

             [binOp Plus "+",
              binOp Minus "-"],

             [binOp LeftShift "<<",
              binOp RightShift ">>"],

             [binOp Less "<",
              binOp LessEqual "<=",
              binOp Greater ">",
              binOp GreaterEqual ">="],

             [binOp Equal "==",
              binOp NotEqual "!="],

             [binOp BitAnd "&"],
             [binOp Xor "^"],
             [binOp BitOr "|"]]
  where prefixOp constructor sym = Prefix (UnaryOp constructor <$ symbol sym)
        binOp constructor sym = InfixL ((BinOp constructor) <$ symbol sym)