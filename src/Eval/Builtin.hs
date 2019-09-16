module Eval.Builtin where 

import AST

import Data.Array.MArray
import Control.Monad

builtinFunctions = [stringToCharArray, println]

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
println' vals = do 
  mapM_ (putStrLn <=< showC0Value) vals 
  return C0VoidVal