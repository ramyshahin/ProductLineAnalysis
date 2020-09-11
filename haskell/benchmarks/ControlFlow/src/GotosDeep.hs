-- Return Average code analysis
-- Returning the average number of goto statements per label
module GotosDeep where

import SPL
import VCFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace
import Data.Ratio

find :: Var Int -> [Var Int] -> Var Bool
find n _ns  = case _ns of [] -> (False ^| ttPC)
                          (h:_t) -> liftedCond (((==) ^| ttPC) <*> (h) <*> (n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) _t)

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
        
isGoto :: Var CFGNode -> Var Bool
isGoto n  = let case0 __cntxt__ = (True ^| __cntxt__)
                split0 __dummy__ = case __dummy__ of CFGStat (CGoto _ _) -> ()
                case1 __cntxt__ = (False ^| __cntxt__)
                split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CGoto _ _) -> 0
                                                                                                                                  _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isLabel :: Var CFGNode -> Var Bool
isLabel n  = let case0 __cntxt__ = (True ^| __cntxt__)
                 split0 __dummy__ = case __dummy__ of CFGStat (CLabel _ _ _ _) -> ()
                 case1 __cntxt__ = (False ^| __cntxt__)
                 split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CLabel _ _ _ _) -> 0
                                                                                                                                   _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n  = let case0 __cntxt__ = (True ^| __cntxt__)
                    split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                    case1 __cntxt__ = (True ^| __cntxt__)
                    split1 __dummy__ = case __dummy__ of CFGStat (CBreak _) -> ()
                    case2 __cntxt__ = (True ^| __cntxt__)
                    split2 __dummy__ = case __dummy__ of CFGFuncRoot _ -> ()
                    case3 __cntxt__ = (False ^| __cntxt__)
                    split3 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGDecl _ -> 0
                                                                                                                                      CFGStat (CBreak _) -> 1
                                                                                                                                      CFGFuncRoot _ -> 2
                                                                                                                                      _ -> 3) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2), \__cntxt__ -> (uncurry0 (case3 __cntxt__)) . (liftV split3)]

followSuccessor :: Var CFG -> [Var Int] -> Var CFGNode -> Var Integer
followSuccessor cfg _visited n  = liftedCond (((||) ^| ttPC) <*> ((find (_nID' n) _visited)) <*> ((isFuncCall n))) (\__cntxt__ -> (0 ^| __cntxt__)) (\__cntxt__ -> liftedCond (isReturn (n /^ __cntxt__)) (\__cntxt__ -> (1 ^| __cntxt__)) (\__cntxt__ -> followSuccessors (cfg /^ __cntxt__) ((_nID' (n /^ __cntxt__)) ^: _visited) (n /^ __cntxt__)))

followSuccessors :: Var CFG -> [Var Int] -> Var CFGNode -> Var Integer
followSuccessors cfg _visited n  = let _ss = _succs' cfg n in foldr' (\a b -> ((+) ^| ttPC) <*> (a) <*> (b)) (0 ^| ttPC) (map' (followSuccessor cfg _visited) _ss)

returnAvg :: Var CFG -> Var CFGNode -> Var Integer
returnAvg cfg n  = followSuccessors cfg [_nID' n] n

analyze :: Var CFG -> Var Rational
analyze cfg  = let _ns = _nodes' cfg
                   _ls = filter' isLabel _ns
                   _gs = filter' isGoto _ns
                   labelCount = length' _ls
                   gotoCount = length' _gs in liftedCond (((==) ^| ttPC) <*> (labelCount) <*> ((0 ^| ttPC))) (\__cntxt__ -> (0 ^| __cntxt__)) (\__cntxt__ -> ((%) ^| __cntxt__) <*> (((toInteger ^| __cntxt__) <*> (gotoCount /^ __cntxt__))) <*> (((toInteger ^| __cntxt__) <*> (labelCount /^ __cntxt__))))

