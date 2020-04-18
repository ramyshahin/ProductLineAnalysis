module TokenCountDeep where

import SPL
import Lexer

tokenCount :: Var [CToken] -> Var Int
tokenCount xs  = (liftedCond mkVarT null <*> xs ((mkVarT 0)) ((liftedCond (mkVarT (==)) <*> mkVarT fst <*> (head xs) <*> mkVarT TNil (mkVarT tokenCount <*> (tail xs)) ((mkVarT (+)) <*> (mkVarT 1) <*> mkVarT tokenCount <*> (tail xs)))))
