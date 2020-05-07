-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Checking whether a non-void function has a return statement
module ReturnDeep where

import SPL
import VCFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace

find :: Var Int -> [Var Int] -> Var Bool
find n _ns  = case _ns of [] -> (False ^| ttPC)
                          (h:_t) -> liftedCond (((==) ^| ttPC) <*> (h) <*> (n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) _t)

{-
isCase :: CFGNode -> Bool
isCase n = 
    case ast n of
        CFGStat (CCase _ _ _)   -> True
        _                       -> False
-}

isFnRoot :: Var CFGNode -> Var Bool
isFnRoot n  = let case0 __cntxt__ = (True ^| __cntxt__)
                  split0 __dummy__ = case __dummy__ of CFGFuncRoot _ -> ()
                  case1 __cntxt__ = (False ^| __cntxt__)
                  split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGFuncRoot _ -> 0
                                                                                                                                    _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isReturn :: Var CFGNode -> Var Bool
isReturn n  = let case0 __cntxt__ = (True ^| __cntxt__)
                  split0 __dummy__ = case __dummy__ of CFGStat (CReturn _ _) -> ()
                  case1 __cntxt__ = (False ^| __cntxt__)
                  split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CReturn _ _) -> 0
                                                                                                                                    _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]
        
isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n  = let case0 __cntxt__ = (True ^| __cntxt__)
                    split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                    case1 __cntxt__ = (True ^| __cntxt__)
                    split1 __dummy__ = case __dummy__ of CFGFuncRoot _ -> ()
                    case2 __cntxt__ = (False ^| __cntxt__)
                    split2 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGDecl _ -> 0
                                                                                                                                      CFGFuncRoot _ -> 1
                                                                                                                                      _ -> 2) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

followSuccessor :: Var CFG -> [Var Int] -> Var CFGNode -> Var Bool
followSuccessor cfg _visited n  = liftedCond (((||) ^| ttPC) <*> ((find (_nID' n) _visited)) <*> ((isFuncCall n))) (\__cntxt__ -> (False ^| __cntxt__)) (\__cntxt__ -> liftedCond ((isReturn (n /^ __cntxt__))) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> followSuccessors (cfg /^ __cntxt__) ((_nID' (n /^ __cntxt__)) ^: _visited) (_succs' (cfg /^ __cntxt__) (n /^ __cntxt__))))

followSuccessors :: Var CFG -> [Var Int] -> [Var CFGNode] -> Var Bool
followSuccessors cfg _visited _ns  = foldr' (\a b -> ((||) ^| ttPC) <*> (a) <*> (b)) (False ^| ttPC) (map' (followSuccessor cfg _visited) _ns)

hasReturn :: Var CFG -> Var CFGNode -> Var Bool
hasReturn cfg n  = followSuccessors cfg [] [n]

analyze :: Var CFG -> [Var CFGNode]
analyze cfg  = let _ns = _nodes' cfg
                   fns = filter' isFnRoot _ns in filter' (not' ^. (hasReturn cfg)) fns

