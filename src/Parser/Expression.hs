module Parser.Expression where

import Parser.Lexer 
import Parser.AST

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char

expression :: Parser Expression
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
       BoolLiteral True <$ symbol "true" <|>
       BoolLiteral False <$ symbol "false" <|>
       Identifier <$> identifier 

operators :: [[Operator Parser Expression]]
operators = [[prefixOp Negate "-",
              prefixOp BitNot "~",
              prefixOp BoolNot "!",
              prefixOp PointerDeref "*"],

             [binOp' Multiply "*",
              binOp' Divide "/",
              binOp' Mod "%"],

             [binOp' Plus "+",
              binOp' Minus "-"],

             [binOp' LeftShift "<<",
              binOp' RightShift ">>"],

             [binOp' Less "<",
              binOp' LessEqual "<=",
              binOp' Greater ">",
              binOp' GreaterEqual ">="],

             [binOp Equal "==",
              binOp NotEqual "!="],

             [binOp' BitAnd "&"],
             [binOp' Xor "^"],
             [binOp' BitOr "|"]]
  where prefixOp constructor sym = Prefix (UnaryOp constructor <$ symbol sym)
        binOp constructor sym = InfixL ((BinOp constructor) <$ symbol sym)
        -- This is for operators which have a compound assignment counterpart
        -- This way the + in += doesn't get lexed individually, for example.
        binOp' constructor sym = InfixL ((BinOp constructor) <$ (try $ symbol sym <* notFollowedBy (char '=')))