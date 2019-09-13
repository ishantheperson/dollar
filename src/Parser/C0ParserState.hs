module Parser.C0ParserState where 

import Parser.AST

import qualified Data.Map.Strict as Map
-- import Control.Arrow ((>>>))

data C0ParserState = C0ParserState {
                       knownTypedefs :: Map.Map String C0Type
                     } -- might need lenses *sigh*

--addTypedef name typeValue = 
--  (lift modify) (knownTypedefs >>> 
--    Map.insertWith (\_ _ -> fail "duplicate typedef") name typeValue)