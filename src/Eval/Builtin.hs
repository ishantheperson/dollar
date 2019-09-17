module Eval.Builtin where 

import AST

import Data.Array.MArray
import Control.Monad

import System.Exit (exitFailure)

builtinFunctions = [stringToCharArray, println, c0Assert, c0Error]

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