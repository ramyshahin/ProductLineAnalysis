module TokenCountDeep where

import SPL
import Lexer

tokenCount :: Var [CToken] -> Var Int
tokenCount xs  = liftedCond (null <*> xs) ((mkVarT 0)) (liftedCond ((mkVarT (==)) <*> fst <*> (head xs) <*> mkVarT TNil) (tokenCount (tail <*> xs)) ((mkVarT (+)) <*> (mkVarT 1) <*> tokenCount (tail <*> xs)))
