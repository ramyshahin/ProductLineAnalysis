module TokenCountDeep where

import SPL
import DeepTypes
import Lexer
import Debug.Trace

tokenCount :: Var [CToken] -> Var Int
tokenCount xs  = let case0 = (mkVarT 0)
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 y ys = let   c = (fst' y ==^ (mkVarT TNil))
                                        ret' = (tokenCount ys)
                                        x = (mkVarT 1) +^ ret'
                                        ret = liftedCond c ret' x
                                  in ret
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys)  
                 in  --trace (show xs) $
                     liftedCase xs (\__dummy__ -> case __dummy__ of [] -> 0
                                                                    (y : ys) -> 1) 
                               [\a -> let r = (uncurry0 case0) . (liftV split0) $ a
                                      in r, 
                                \a -> let x = (liftV split1) a
                                          r = case1 (fst' x) (snd' x)
                                      in r]
