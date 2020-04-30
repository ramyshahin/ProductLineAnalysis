-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module CaseTerminationDeep where

import SPL
import VPreludeDeep
import CFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace

find :: Var Int -> Var [Int] -> Var Bool
find n ns  = let case0 __cntxt__ = (False ^| __cntxt__)
                 split0 __dummy__ = case __dummy__ of [] -> ()
                 case1 __cntxt__ h t = liftedCond ((h /^ __cntxt__) ^== (n /^ __cntxt__)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) (t /^ __cntxt__))
                 split1 __dummy__ = case __dummy__ of (h:t) -> (h, t) in liftedCase (ns) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                          (h:t) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]

isCase :: Var CFGNode -> Var Bool
isCase n  = let case0 __cntxt__ = (True ^| __cntxt__)
                split0 __dummy__ = case __dummy__ of CFGStat (CCase _ _ _) -> ()
                case1 __cntxt__ = (False ^| __cntxt__)
                split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CCase _ _ _) -> 0
                                                                                                                                  _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isDefault :: Var CFGNode -> Var Bool
isDefault n  = let case0 __cntxt__ = (True ^| __cntxt__)
                   split0 __dummy__ = case __dummy__ of CFGStat (CDefault _ _) -> ()
                   case1 __cntxt__ = (False ^| __cntxt__)
                   split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CDefault _ _) -> 0
                                                                                                                                     _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isBreak :: Var CFGNode -> Var Bool
isBreak n  = let case0 __cntxt__ = (True ^| __cntxt__)
                 split0 __dummy__ = case __dummy__ of CFGStat (CBreak _) -> ()
                 case1 __cntxt__ = (False ^| __cntxt__)
                 split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CBreak _) -> 0
                                                                                                                                   _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n  = let case0 __cntxt__ = (True ^| __cntxt__)
                    split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                    case1 __cntxt__ = (False ^| __cntxt__)
                    split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGDecl _ -> 0
                                                                                                                                      _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

followSuccessor :: Var CFG -> Var [Int] -> Var CFGNode -> Var Bool
followSuccessor cfg visited n  = liftedCond ((find ((_nID ^| ttPC) <*> n) visited) ^|| (isBreak n) ^|| (isFuncCall n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> liftedCond ((isCase (n /^ __cntxt__)) ^|| (isDefault (n /^ __cntxt__))) (\__cntxt__ -> (False ^| __cntxt__)) (\__cntxt__ -> followSuccessors (cfg /^ __cntxt__) (((_nID ^| __cntxt__) <*> (n /^ __cntxt__)) ^: (visited /^ __cntxt__)) ((_succs ^| __cntxt__) <*> (cfg /^ __cntxt__) <*> (n /^ __cntxt__))))
 
followSuccessors :: Var CFG -> Var [Int] -> Var [CFGNode] -> Var Bool
followSuccessors cfg visited ns  = foldr' (\a b -> a ^&& b) (True ^| ttPC) (map' (followSuccessor cfg visited) ns)

terminatedCase :: Var CFG -> Var CFGNode -> Var Bool
terminatedCase cfg n  = let ss = filter' (not' ^. isCase) ((_succs ^| ttPC) <*> cfg <*> n) in followSuccessors cfg (mkVarT []) ss

analyze :: Var CFG -> Var [CFGNode]
analyze cfg  = let ns = (nodes ^| ttPC) <*> cfg
                   cases = filter' isCase ns in filter' (not' ^. (terminatedCase cfg)) cases

