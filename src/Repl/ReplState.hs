module Repl.ReplState where 

import Eval.Context 
import Control.Monad.Trans.State.Strict 

type ReplStateT = StateT ReplState 
data 