module Eval.Context where 

import AST
import Eval.C0Value

import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Strict

import qualified Data.Map.Strict as Map 

type VarMap = Map.Map String C0Value 

data Context = Context { 
                 getCurrentScope :: VarMap, 
                 getParentScope :: Maybe Context 
               } deriving Show 

emptyContext = Context Map.empty Nothing 

-- throw IO error if var doesnt exist 
lookupVar name = go name <$> get
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

pushScope :: Context -> Context 
pushScope c = Context Map.empty (Just c) 

popScope :: Context -> Context 
popScope = fromJust . getParentScope 

fromMap :: VarMap -> Context 
fromMap = flip Context Nothing

getAllVars :: Context -> [VariableDecl]
getAllVars c = map (flip VariableDecl C0Void) (Map.keys (getCurrentScope c)) ++ rest 
  where rest = case getParentScope c of 
                 Nothing -> []
                 Just parent -> getAllVars parent 