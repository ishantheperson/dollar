{-# LANGUAGE LambdaCase #-}
module Eval.C0Value where 

import AST

import Data.List (intercalate)
import Data.Int 
import Data.IORef
import Data.Array.IO 
import Data.Array.MArray 

data C0Value = C0StringVal String 
             | C0CharVal Char 
             | C0IntVal Int32 
             | C0BoolVal Bool 
             | C0VoidVal 

             | C0PointerVal C0Type (Maybe (IORef C0Value)) -- ^ Nothing indicates NULL
             | C0ArrayVal C0Type (IOArray Int32 C0Value) -- ^ C0 arrays cannot be NULL
                 deriving (Show, Eq)

showC0Value :: C0Value -> IO String
showC0Value = \case 
  C0StringVal s -> return s 
  C0CharVal c -> return [c]
  C0IntVal i -> return $ show i -- TODO: number formats via Reader monad 
  C0BoolVal True -> return "true"
  C0BoolVal False -> return "false"
  C0VoidVal -> return "(void)"
  C0PointerVal _ Nothing -> return "NULL"
  C0ArrayVal _ a -> do arrayElems <- getElems a
                       strings <- mapM showC0Value arrayElems
                       return $ "{" ++ intercalate "," strings ++ "}"

c0DefaultValue :: C0Type -> C0Value
c0DefaultValue = \case 
  C0Int -> C0IntVal 0
  C0String -> C0StringVal ""
  C0Bool -> C0BoolVal False 
  C0Char -> C0CharVal '\0'
  C0Typedef s -> error $ "Please use full type name instead of type alias (temporary)"
  C0Pointer t -> C0PointerVal t Nothing 
  C0Array t -> C0ArrayVal t undefined -- FIXME: maybe change it to Maybe (...)

instance Show (IORef a) where 
  show = const "<io ref>"

instance Show (IOArray a b) where 
  show = const "<io array>"