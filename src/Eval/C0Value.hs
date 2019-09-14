module Eval.C0Value where 

import AST

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

instance Show (IORef a) where 
  show = const "<io ref>"

instance Show (IOArray a b) where 
  show = const "<io array>"