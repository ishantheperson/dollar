{-# LANGUAGE LambdaCase #-}
module Eval.Context where 

import AST
--import Eval.C0Value

import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Strict

import qualified Data.Map.Strict as Map 

type VarMap = Map.Map String C0Value 

data Context = Context { 
                 getCurrentScope :: VarMap, 
                 getParentScope :: Maybe Context 
               } deriving Show 

data EvalInfo = EvalInfo {
                  getContext :: Context,
                  getEvalTypedefs :: [(String, C0Type)],
                  getEvalStructs :: [(String, [(String, C0Type)])]
                } deriving Show 

emptyContext = Context Map.empty Nothing 
emptyEvalInfo = EvalInfo emptyContext [] [] 

-- throw IO error if var doesnt exist 
lookupVar name = go name <$> gets getContext
  where go name context = 
          case Map.lookup name (getCurrentScope context) of 
            Just v -> v 
            -- crashes if variable doesnt exist, thats the type checkers job :) 
            Nothing -> go name (fromJust $ getParentScope context) 

insertVar :: String -> C0Value -> EvalInfo -> EvalInfo
insertVar name val info = 
  let context = getContext info 
      scope = getCurrentScope context 
      newMap = Map.insert name val scope 
  in info { getContext = context { getCurrentScope = newMap } }

updateVar :: String -> C0Value -> EvalInfo -> EvalInfo
updateVar name val info = info { getContext = go (getContext info) }
  where go :: Context -> Context 
        go context = 
          let scope = getCurrentScope context in 
          case Map.lookup name (getCurrentScope context) of 
              Just _ -> context { getCurrentScope = Map.insert name val scope }
              Nothing -> context { getParentScope = Just $ go (fromJust $ getParentScope context) }

pushScope :: EvalInfo -> EvalInfo 
pushScope info = 
  let context = getContext info 
  in info { getContext = Context Map.empty (Just context) }

popScope :: EvalInfo -> EvalInfo
popScope info = 
  let context = getContext info 
  in info { getContext = fromJust . getParentScope $ context }

fromMap :: VarMap -> Context 
fromMap = flip Context Nothing

getAllVars :: EvalInfo -> [VariableDecl]
getAllVars info = 
  let context = getContext info 
  in go (Just context)
  where go = \case Just c -> map (flip VariableDecl C0Void) (Map.keys (getCurrentScope c)) ++ go (getParentScope c)
                   Nothing -> []  

-- getAllVars :: EvalInfo  -> [VariableDecl]
-- getAllVars info =
--   let context = getContext info  
--   in map (flip VariableDecl C0Void) (Map.keys (getCurrentScope context)) ++ rest 
--   where rest = case getParentScope c of 
--                  Nothing -> []
--                  Just parent -> getAllVars parent 