module TokenCountDeep where

import SPL
import VPreludeDeep
import Lexer

tokenCount :: Var [CToken] -> Var Int
tokenCount xs  = let case0 = (mkVarT 0)
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 y ys = liftedCond (fst' y ^== (mkVarT TNil)) (tokenCount ys) ((mkVarT 1) ^+ tokenCount ys)
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                  (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]
