{-# LANGUAGE LambdaCase #-}
module Parser.C0ParserState where 

import Parser.AST

--import qualified Data.Map.Strict as Map
-- import Control.Arrow ((>>>))

data ParsingMode = Regular | LineContract | BlockContract deriving Show
isParsingContract = \case Regular -> False 
                          _ -> True 

data C0ParserState = C0ParserState {
                       parsingMode :: ParsingMode 
                     } deriving Show -- might need lenses *sigh*

defaultState = C0ParserState Regular 

--addTypedef name typeValue = 
--  (lift modify) (knownTypedefs >>> 
--    Map.insertWith (\_ _ -> fail "duplicate typedef") name typeValue)