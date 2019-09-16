{-# LANGUAGE LambdaCase #-}
module AST where 

import Data.List (intercalate)
import Data.Int 
import Data.IORef
import Data.Array.IO 
import Data.Array.MArray 

data VariableDecl = VariableDecl { varName :: String, varType :: C0Type } deriving Show 

data FunctionBody = C0FunctionBody [Statement] | NativeFunctionBody ([C0Value] -> IO C0Value)
instance Show FunctionBody where 
  show = \case 
    C0FunctionBody s -> "C0FunctionBody " ++ show s 
    NativeFunctionBody _ -> "(Builtin- haskell function)"

data Function = Function { functionType :: C0Type, 
                           functionName :: String, 
                           functionArgDecls :: [VariableDecl],
                           functionContracts :: [Contract],
                           functionBody :: FunctionBody } deriving Show 

showFunctionHeader :: Function -> String 
showFunctionHeader f = 
  show (functionType f) ++ " " ++ functionName f ++ "(" ++ intercalate ", " (map (\(VariableDecl n t) -> show t ++ " " ++ n) (functionArgDecls f)) ++ ")"

data Expression = -- Terms
                  IntConstant Integer 
                | StringLiteral String 
                | CharLiteral Char 
                | BoolLiteral Bool 
                | Identifier String

                | FunctionCall Expression [Expression]
                | Ternary Expression Expression Expression

                | ArrayAccess Expression Expression

                | StructDotAccess Expression String 
                | StructArrowAccess Expression String 

                | Alloc C0Type
                | AllocArray C0Type Expression  
                | NullConst

                | ContractLength Expression
                | HasTag C0Type Expression
                | ContractResult 
                
                  -- Expression 
                | BinOp BinOperator Expression Expression
                | UnaryOp UnaryOperator Expression
                    deriving Show
-- Note that ++ and -- are missing because they are actually
-- not expressions, instead they can only be used as statements in C0

data BinOperator = ArithOp ArithOperator | BoolOp BoolOperator | CmpOp CmpOperator deriving Show  

data ArithOperator = Plus | Minus | Multiply | Mod | Divide | BitAnd 
                   | BitOr | Xor | LeftShift | RightShift deriving Show 

data BoolOperator = BoolAnd | BoolOr deriving Show 
data CmpOperator = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual deriving Show 

data UnaryOperator =
    -- | Integer negation e.g. -1
    Negate
    -- | Bitwise negation e.g. ~0x33
  | BitNot 
  | BoolNot
  | PointerDeref
  deriving Show

type Typedef = (String, C0Type)
data C0Type = C0Int -- prefixed with C0 to avoid name collisions with Haskell types...
            | C0Char
            | C0String
            | C0Bool
            | C0Void
            | C0Typedef String C0Type
            | C0Pointer C0Type
            | C0Struct String 
            | C0Array C0Type deriving (Eq) 
            -- also need function ptr types

instance Show C0Type where 
  show = \case 
    C0Int -> "int"
    C0Char -> "char"
    C0String -> "string"
    C0Bool -> "bool"
    C0Void -> "void"
    C0Typedef s _ -> s 
    C0Pointer t -> show t ++ "*"
    C0Struct s -> "struct " ++ s 
    C0Array t -> show t ++ "[]"

data Statement = VariableDeclStmnt VariableDecl
               | DeclAssign VariableDecl Expression
               | Assign Expression Expression
               -- | Increment Expression
               -- | Decrement Expression
               | IfStatement Expression Statement (Maybe Statement)
               -- Contracts need to be added to loops here
               | WhileLoop Expression [Contract] Statement
               | ForLoop Statement Expression (Maybe Statement) [Contract] Statement 
               | Return (Maybe Expression)
               | Assert Expression
               | Error Expression
               | FunctionCallStmnt Expression
               | StatementContract [Contract]
               | StatementBlock [Statement] deriving Show -- technically "3;" is valid here too 

data ContractType = AssertContract | LoopInvariant | Requires | Ensures deriving (Show, Eq)
data Contract = Contract { getContractType :: ContractType, getContractBody :: Expression } deriving Show

data C0Value = C0StringVal String 
             | C0CharVal Char 
             | C0IntVal Int32 
             | C0BoolVal Bool 
             | C0VoidVal 

             | C0PointerVal C0Type (Maybe (IORef C0Value)) -- ^ Nothing indicates NULL
             | C0ArrayVal C0Type (IOArray Int32 C0Value) -- ^ C0 arrays cannot be NULL
                 deriving (Show, Eq)

-- Because 
showC0Value :: C0Value -> IO String
showC0Value = \case 
  C0StringVal s -> return $ show s 
  C0CharVal c -> return $ show c
  C0IntVal i -> return $ show i -- TODO: number formats via Reader monad 
  C0BoolVal True -> return "true"
  C0BoolVal False -> return "false"
  C0VoidVal -> return "(void)"
  C0PointerVal _ Nothing -> return "NULL"
  C0ArrayVal _ a -> do arrayElems <- getElems a
                       strings <- mapM showC0Value arrayElems
                       return $ "{" ++ intercalate ", " strings ++ "}"

c0DefaultValue :: C0Type -> C0Value
c0DefaultValue = \case 
  C0Int -> C0IntVal 0
  C0String -> C0StringVal ""
  C0Bool -> C0BoolVal False 
  C0Char -> C0CharVal '\0'
  C0Typedef _ t -> c0DefaultValue t 
  C0Pointer t -> C0PointerVal t Nothing 
  C0Array t -> C0ArrayVal t undefined -- FIXME: maybe change it to Maybe (...)  

instance Show (IORef a) where 
  show = const "<io ref>"

instance Show (IOArray a b) where 
  show = const "<io array>"