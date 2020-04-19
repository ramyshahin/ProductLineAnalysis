module TokenCountDeep where

import SPL
import Lexer

tokenCount :: Var [CToken] -> Var Int
tokenCount xs  = liftedCond (null' xs) ((mkVarT 0)) (liftedCond (fst' (head' xs) |==| (mkVarT TNil)) (tokenCount (tail' xs)) ((mkVarT 1) |+| tokenCount (tail' xs)))
