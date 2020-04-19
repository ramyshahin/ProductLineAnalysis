module TokenCountDeep where

import SPL
import Lexer

tokenCount :: Var [CToken] -> Var Int
tokenCount xs  = liftedCond ((mkVarT null) <*> xs) ((mkVarT 0)) (liftedCond ((mkVarT (==)) <*> ((mkVarT fst) <*> ((mkVarT head) <*> xs)) <*> ((mkVarT TNil))) (tokenCount ((mkVarT tail) <*> xs)) ((mkVarT (+)) <*> ((mkVarT 1)) <*> (tokenCount ((mkVarT tail) <*> xs))))
