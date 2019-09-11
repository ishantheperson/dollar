module Parser.AST where 
  
import Data.Int

data VariableDecl = VariableDecl { varName :: String, varType :: C0Type } deriving Show 

data Function = Function { functionType :: C0Type, 
                           functionName :: String, 
                           functionArgDecls :: [VariableDecl] } deriving Show

data Expression = -- Terms
                  IntConstant Int32 
                | StringLiteral String 
                | CharLiteral Char 
                | BoolLiteral Bool 
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

data C0Type = C0Int 
            | C0Char
            | C0String
            | C0Bool
            | C0Void
            | C0Typedef String 
            | C0Pointer C0Type
            | C0Array C0Type deriving Show 
            -- also need function ptr types
