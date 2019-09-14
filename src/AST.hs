module AST where 

data VariableDecl = VariableDecl { varName :: String, varType :: C0Type } deriving Show 

data Function = Function { functionType :: C0Type, 
                           functionName :: String, 
                           functionArgDecls :: [VariableDecl],
                           functionContracts :: [Contract],
                           functionBody :: [Statement] } deriving Show

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

data C0Type = C0Int -- prefixed with C0 to avoid name collisions with Haskell types...
            | C0Char
            | C0String
            | C0Bool
            | C0Void
            | C0Typedef String 
            | C0Pointer C0Type
            | C0Struct String 
            | C0Array C0Type deriving (Show, Eq) 
            -- also need function ptr types

--unifyTypes a b = do 


data Statement = VariableDeclStmnt VariableDecl
               | DeclAssign VariableDecl Expression
               | Assign Expression Expression
               | Increment Expression
               | Decrement Expression
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

