{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables #-}
module Eval where 

import AST
import Eval.C0Value
import Eval.Context 

import Data.Bits 
import Data.Int 
import qualified Data.Map.Strict as Map 

import Data.Maybe (fromMaybe)

import Control.Arrow

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except 
import Control.Monad.Trans.State.Strict

--evalFunction :: Function -> [C0Value] -> IO () 
evalFunction f args = 
  let initialContext = fromMap . Map.fromList $ zip (map varName (functionArgDecls f)) args 
      requires = filter ((==) Requires . getContractType) (functionContracts f)
      ensures = filter ((==) Ensures . getContractType) (functionContracts f)
      
  in runStateT (runFunction initialContext requires ensures) initialContext
  where runFunction c requires ensures = do 
          put c :: StateT Context IO ()
          -- Contracts 
          preconditionsMet <- c0BoolAnd <$> mapM (evalE . getContractBody) requires
          when (not preconditionsMet) (liftIO $ ioError (userError "Requires failed"))

          Left retVal <- runExceptT (traverse evalS (functionBody f))
          modify' $ insertVar "\\result" retVal

          postconditionsMet <- c0BoolAnd <$> mapM (evalE . getContractBody) ensures 
          when (not postconditionsMet) (liftIO $ ioError (userError "Ensures failed"))

          return retVal

        c0BoolAnd = \case [] -> True 
                          (C0BoolVal b):xs -> b && c0BoolAnd xs 

-- | Returns (Left v1) to indicate the function returned
--   (Right ()) to indicate otherwise. Eventually we will
--   also need to pass other functions 
evalS :: Statement -> ExceptT C0Value (StateT Context IO) ()
evalS = \case   
  VariableDeclStmnt (varName -> n) -> lift $ modify (insertVar n undefined)
  DeclAssign (varName -> n) value -> lift (evalE value) >>= \c -> lift $ modify (insertVar n c) 
  -- We need special treatment of lvalues here
  --Assign n value -> lift (evalE value) >>= \c -> lift $ modify (insertVar n c) 

  Return Nothing -> throwE C0VoidVal 
  Return (Just value) -> (lift $ evalE value) >>= throwE 

evalE :: Expression -> StateT Context IO C0Value
evalE = \case 
  IntConstant i -> return $ C0IntVal (fromInteger i)
  StringLiteral s -> return $ C0StringVal s 
  CharLiteral c -> return $ C0CharVal c 
  BoolLiteral b -> return $ C0BoolVal b 
  Identifier v -> lookupVar v 
  ContractResult -> lookupVar "\\result"

  -- This can be moved to another file 
  BinOp (ArithOp op) lhs rhs -> do 
    C0IntVal a <- evalE lhs 
    C0IntVal b <- evalE rhs 

    return . C0IntVal $ (getArithOp op) a b 

  -- This means == doesn't work on pointers right now
  BinOp (CmpOp Equal) lhs rhs -> do
    a <- evalE lhs
    b <- evalE rhs 

    return . C0BoolVal $ (a == b)

  BinOp (CmpOp NotEqual) lhs rhs -> do
    a <- evalE lhs
    b <- evalE rhs 

    return . C0BoolVal $ (a /= b)

  BinOp (CmpOp op) lhs rhs -> do 
    C0IntVal a <- evalE lhs 
    C0IntVal b <- evalE rhs 

    return . C0BoolVal $ (getCmpOp op) a b 

getArithOp :: ArithOperator -> (Int32 -> Int32 -> Int32)
getArithOp = \case 
  Plus -> (+)
  Minus -> (-)
  Multiply -> (*)
  Divide -> div 
  Mod -> mod 

  BitAnd -> (.&.)
  BitOr -> (.|.)
  Xor -> xor 
  LeftShift -> shiftL'
  RightShift -> shiftR'

  where shiftL' a b = shiftL a (fromIntegral b)
        shiftR' a b = shiftR a (fromIntegral b)

getCmpOp :: CmpOperator -> (Int32 -> Int32 -> Bool)
getCmpOp = \case 
  Equal -> (==)
  NotEqual -> (/=)
  Less -> (<)
  LessEqual -> (<=)
  Greater -> (>)
  GreaterEqual -> (>=)  

getBoolOp :: BoolOperator -> (Bool -> Bool -> Bool)
getBoolOp = \case 
  BoolAnd -> (&&)
  BoolOr -> (||)
