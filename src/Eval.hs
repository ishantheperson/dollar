{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Eval where 

import AST
import Eval.C0Value

import Data.Bits 
import Data.Int 

import Data.Maybe (fromJust, fromMaybe)

import Control.Arrow

import Control.Monad.Trans
import Control.Monad.Trans.Except 
import Control.Monad.Trans.State.Strict

import qualified Data.Map.Strict as Map 

type VarMap = Map.Map String C0Value 

data Context = Context { 
                 getCurrentScope :: VarMap, 
                 getParentScope :: Maybe Context 
               } deriving Show 

emptyContext = Context Map.empty Nothing 

-- throw IO error if var doesnt exist 
lookupVar name = do 
  context <- get
  return $ go name context 
  
  where go name context = 
          case Map.lookup name (getCurrentScope context) of 
            Just v -> v 
            -- crashes if variable doesnt exist, thats the type checkers job :) 
            Nothing -> go name (fromJust $ getParentScope context) 

insertVar :: String -> C0Value -> Context -> Context 
insertVar name val context = 
  let scope = getCurrentScope context 
      newMap = Map.insert name val scope 
  in context { getCurrentScope = newMap }

-- | Returns (Left v1) to indicate the function returned
--   (Right ()) to indicate otherwise 
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

  BinOp (ArithOp op) lhs rhs -> do 
    C0IntVal a <- evalE lhs 
    C0IntVal b <- evalE rhs 

    return . C0IntVal $ (getArithOp op) a b 

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

getBoolOp :: BoolOperator -> (Bool -> Bool -> Bool)
getBoolOp = \case 
  BoolAnd -> (&&)
  BoolOr -> (||)
