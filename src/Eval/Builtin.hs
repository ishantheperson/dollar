{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Eval.Builtin (builtinFunctions, 
  string, conio, util) where 

import AST

import Data.Int
import Data.Array.MArray
import Control.Monad

import System.Exit (exitFailure)

builtinFunctions = 
  [c0Assert, c0Error] ++ -- Builtins
  string ++
  conio ++ 
  util 

string = [stringToCharArray]
conio = [c0print, println, printInt]
util = [c0abs, intMax, intMin]

stringToCharArray :: Function
stringToCharArray = Function {
  functionType = C0Array C0Char,
  functionName = "string_to_chararray",
  functionArgDecls = [VariableDecl "s" C0String],
  functionBody = NativeFunctionBody stringToCharArray',
  functionContracts = []
  --functionContracts = [Contract Ensures (BinOp (CmpOp Equal) (ContractLength ContractResult) ())]
}

stringToCharArray' :: [C0Value] -> IO C0Value
stringToCharArray' [C0StringVal s] = 
  let chars = map C0CharVal  (s ++ "\0") 
  in C0ArrayVal C0Char <$> newListArray (0, (fromIntegral $ length chars) - 1) chars 

println :: Function
println = Function {
  functionType = C0Void,
  functionName = "println",
  functionArgDecls = [VariableDecl "s" C0String],
  functionBody = NativeFunctionBody println',
  functionContracts = []
}

println' :: [C0Value] -> IO C0Value
println' [C0StringVal s] = do 
  C0VoidVal <$ putStrLn s 
  --mapM_ (putStrLn <=< showC0Value) vals 
  --return C0VoidVal

c0print = Function {
  functionType = C0Void,
  functionName = "print",
  functionArgDecls = [VariableDecl "s" C0String],
  functionBody = NativeFunctionBody c0print',
  functionContracts = []
}

c0print' [C0StringVal s] = C0VoidVal <$ putStr s 

printInt = Function {
  functionType = C0Void,
  functionName = "printint",
  functionArgDecls = [VariableDecl "i" C0Int],
  functionBody = NativeFunctionBody c0PrintInt',
  functionContracts = []
}

c0PrintInt' [C0IntVal i] = C0VoidVal <$ putStr (show i)

c0Assert = Function {
  functionType = C0Void,
  functionName = "assert",
  functionArgDecls = [VariableDecl "condition" C0Bool],
  functionContracts = [],
  functionBody = NativeFunctionBody c0Assert'
}

c0Assert' [C0BoolVal b] = do 
  when (not b) $ error "Assertion failed"
  return C0VoidVal

c0Error = Function {
  functionType = C0Void,
  functionName = "error",
  functionArgDecls = [VariableDecl "message" C0String],
  functionContracts = [],
  functionBody = NativeFunctionBody c0Error'
}

c0Error' [C0StringVal b] = do 
  putStr "\x1b[31;1m" -- bold red 
  putStr "ERROR: "
  putStr "\x1b[0m"

  putStrLn b 
  exitFailure

c0abs = Function {
  functionType = C0Int,
  functionName = "abs",
  functionArgDecls = [VariableDecl "i" C0Int],
  functionContracts = [], -- todo 
  --functionContracts = [Ensures ()],
  functionBody = NativeFunctionBody $ \[C0IntVal i] -> return $ C0IntVal $ abs i 
}

intMax = Function {
  functionType = C0Int,
  functionName = "int_max",
  functionArgDecls = [],
  functionContracts = [], -- todo 
  --functionContracts = [Ensures ()],
  functionBody = NativeFunctionBody $ \[] -> return $ C0IntVal (maxBound :: Int32)
}

intMin = Function {
  functionType= C0Int,
  functionName = "int_min",
  functionArgDecls = [],
  functionContracts = [], -- todo 
  --functionContracts = [Ensures ()],
  functionBody = NativeFunctionBody $ \[] -> return $ C0IntVal (minBound :: Int32)
}