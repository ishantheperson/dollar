{-# LANGUAGE LambdaCase #-}
module Parser.C0ParserState where 

import AST

--import qualified Data.Map.Strict as Map
-- import Control.Arrow ((>>>))

data ParsingMode = Regular | LineContract | BlockContract deriving Show
isParsingContract = \case Regular -> False 
                          _ -> True 

data C0ParserState = C0ParserState {
                       parsingMode :: ParsingMode
--                       knownFunctions :: [Function],
--                       knownTypedefs :: []
                     } deriving Show -- might need lenses *sigh*

defaultState = C0ParserState Regular 

setParserMode mode oldState = oldState { parsingMode = mode }