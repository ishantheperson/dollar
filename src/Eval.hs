{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables #-}
module Eval where 

import AST
import Eval.Context 
import Eval.Builtin

import Data.Bits 
import Data.Int 
import Data.Array.IO
import qualified Data.Map.Strict as Map 

--import Data.Maybe (fromMaybe)

--import Control.Arrow

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except 
import Control.Monad.Trans.State.Strict

type Evaluator = StateT Context IO
runEvalT = flip runStateT emptyContext 


--evalFunction :: Function -> [C0Value] -> IO () 
evalFunction f fs args = 
  let initialContext = fromMap . Map.fromList $ zip (map varName (functionArgDecls f)) args 
      requires = filter ((==) Requires . getContractType) (functionContracts f)
      ensures = filter ((==) Ensures . getContractType) (functionContracts f)
      
  in runStateT (runFunction initialContext requires ensures) initialContext
  where runFunction c requires ensures = do 
          put c :: Evaluator ()
          -- Contracts 
          preconditionsMet <- c0BoolAnd <$> mapM (evalE fs . getContractBody) requires
          when (not preconditionsMet) (liftIO $ ioError (userError "Requires failed"))

          let (C0FunctionBody interpertedFunctionBody) = functionBody f 
          Left retVal <- case functionBody f of 
                           NativeFunctionBody fb -> Left <$> liftIO (fb args)
                           C0FunctionBody fb -> runExceptT (traverse (evalS fs) (interpertedFunctionBody))

          --Left retVal <- runExceptT (traverse (evalS fs) (interpertedFunctionBody))
          modify' $ insertVar "\\result" retVal

          postconditionsMet <- c0BoolAnd <$> mapM (evalE fs . getContractBody) ensures 
          when (not postconditionsMet) (liftIO $ ioError (userError "Ensures failed"))

          return retVal

        c0BoolAnd = \case [] -> True 
                          (C0BoolVal b):xs -> b && c0BoolAnd xs 

-- | Returns (Left v1) to indicate the function returned
--   (Right ()) to indicate otherwise. 
--   Reader monad might help...
evalS :: [Function] -> Statement -> ExceptT C0Value Evaluator ()
evalS fs = \case   
  VariableDeclStmnt (varName -> n) -> lift $ modify (insertVar n undefined) -- wow laziness at work 
  DeclAssign (varName -> n) value -> lift (evalE fs value) >>= \c -> lift $ modify (insertVar n c) 
  -- We need special treatment of lvalues here
  Assign (Identifier name) rhs -> do 
    val <- lift $ evalE fs rhs 
    lift $ modify (insertVar name val)

  Assign (ArrayAccess arrayExp indexExp) rhs -> do 
    C0ArrayVal _ array <- lift $ evalE fs arrayExp 
    C0IntVal i <- lift $ evalE fs indexExp
    val <- lift $ evalE fs rhs 

    liftIO $ writeArray array i val 

  Return Nothing -> throwE C0VoidVal 
  Return (Just value) -> (lift $ evalE fs value) >>= throwE 
  ForLoop initStatement guardExpression iterationStatement contracts body -> do 
    evalS fs initStatement
    let loop = do 
          -- TODO: run contracts here
          C0BoolVal cond <- lift $ evalE fs guardExpression
          when cond $ do 
            evalS fs body 
            case iterationStatement of 
              Just s -> evalS fs s 
              Nothing -> return () 
            loop 
  
    loop 

  FunctionCallStmnt e -> () <$ (lift $ evalE fs e)
  other -> error $ "unsupported " ++ show other 

evalE :: [Function] -> Expression -> Evaluator C0Value
evalE fs = \case 
  IntConstant i -> return $ C0IntVal (fromInteger i)
  StringLiteral s -> return $ C0StringVal s 
  CharLiteral c -> return $ C0CharVal c 
  BoolLiteral b -> return $ C0BoolVal b 
  Identifier v -> lookupVar v 
  ContractResult -> lookupVar "\\result"
  Ternary e t f -> do 
    C0BoolVal b <- evalE fs e 
    evalE fs $ if b then t else f  

  AllocArray t numExp -> do 
    C0IntVal n <- evalE fs numExp
    liftIO $ C0ArrayVal t <$> newArray (0, n - 1) (c0DefaultValue t)

  ArrayAccess arrayExp indexExp -> do 
    C0ArrayVal t a <- evalE fs arrayExp
    C0IntVal n <- evalE fs indexExp 

    liftIO $ readArray a n 

  ContractLength a -> do 
    C0ArrayVal _ array <- evalE fs a 
    C0IntVal . snd <$> liftIO (getBounds array) 

  FunctionCall (Identifier fName) args -> do 
    let f = findFunction fName fs 
    argVals <- traverse (evalE fs) args 
    fst <$> (liftIO $ evalFunction f fs argVals)

  -- This can be moved to another file 
  BinOp (ArithOp op) lhs rhs -> do 
    C0IntVal a <- evalE fs lhs 
    C0IntVal b <- evalE fs rhs 

    return . C0IntVal $ (getArithOp op) a b 

  BinOp (CmpOp Equal) lhs rhs -> do
    a <- evalE fs lhs
    b <- evalE fs rhs 

    return . C0BoolVal $ (a == b)

  BinOp (CmpOp NotEqual) lhs rhs -> do
    a <- evalE fs lhs
    b <- evalE fs rhs 

    return . C0BoolVal $ (a /= b)

  BinOp (CmpOp op) lhs rhs -> do 
    C0IntVal a <- evalE fs lhs 
    C0IntVal b <- evalE fs rhs 

    return . C0BoolVal $ (getCmpOp op) a b 

  BinOp (BoolOp op) lhs rhs -> do 
    C0BoolVal a <- evalE fs lhs 
    C0BoolVal b <- evalE fs rhs 

    return . C0BoolVal $ (getBoolOp op) a b 

  x -> error $ "not yet implemented: " ++ show x 
  where findFunction name = head . filter ((==) name . functionName)

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
