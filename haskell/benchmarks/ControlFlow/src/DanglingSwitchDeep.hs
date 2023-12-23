-- Dangling switch code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module DanglingSwitchDeep where
{-
import SPL
import VCFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace
--import qualified Data.MultiMap as M

find :: Var Int -> [Var Int] -> Var Bool
find n _ns  = case _ns of [] -> (False ^| ttPC)
                          (h:_t) -> liftedCond (((==) ^| ttPC) <*> (h) <*> (n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) _t)

isSwitch :: Var CFGNode -> Var Bool
isSwitch n  = let case0 __cntxt__ = (True ^| __cntxt__)
                  split0 __dummy__ = case __dummy__ of CFGStat (CSwitch _ _ _) -> ()
                  case1 __cntxt__ = (False ^| __cntxt__)
                  split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CSwitch _ _ _) -> 0
                                                                                                                                    _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isCase :: Var CFGNode -> Var Bool
isCase n  = let case0 __cntxt__ = (True ^| __cntxt__)
                split0 __dummy__ = case __dummy__ of CFGStat (CCase _ _ _) -> ()
                case1 __cntxt__ = (True ^| __cntxt__)
                split1 __dummy__ = case __dummy__ of CFGStat (CDefault _ _) -> ()
                case2 __cntxt__ = (False ^| __cntxt__)
                split2 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CCase _ _ _) -> 0
                                                                                                                                  CFGStat (CDefault _ _) -> 1
                                                                                                                                  _ -> 2) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

isDecl :: Var CFGNode -> Var Bool
isDecl n  = let case0 __cntxt__ = (True ^| __cntxt__)
                split0 __dummy__ = case __dummy__ of CFGVarDecl _ -> ()
                case1 __cntxt__ = (True ^| __cntxt__)
                split1 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                case2 __cntxt__ = (False ^| __cntxt__)
                split2 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGVarDecl _ -> 0
                                                                                                                                  CFGDecl _ -> 1
                                                                                                                                  _ -> 2) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n  = let case0 __cntxt__ = (True ^| __cntxt__)
                    split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                    case1 __cntxt__ = (True ^| __cntxt__)
                    split1 __dummy__ = case __dummy__ of CFGFunc _ -> ()
                    case2 __cntxt__ = (False ^| __cntxt__)
                    split2 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGDecl _ -> 0
                                                                                                                                      CFGFunc _ -> 1
                                                                                                                                      _ -> 2) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

followSuccessor :: Var CFG -> [Var Int] -> Var CFGNode -> Var Bool
followSuccessor cfg _visited n  = liftedCond (((||) ^| ttPC) <*> ((find (_nID' n) _visited)) <*> (((||) ^| ttPC) <*> ((isCase n)) <*> ((isFuncCall n)))) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> liftedCond (isDecl (n /^ __cntxt__)) (\__cntxt__ -> followSuccessors (cfg /^ __cntxt__) ((_nID' (n /^ __cntxt__)) ^: _visited) (_succs' (cfg /^ __cntxt__) (n /^ __cntxt__))) (\__cntxt__ -> (False ^| __cntxt__)))

followSuccessors :: Var CFG -> [Var Int] -> [Var CFGNode] -> Var Bool
followSuccessors cfg visited ns  = foldr' (\a b -> ((&&) ^| ttPC) <*> (a) <*> (b)) (True ^| ttPC) (map' (followSuccessor cfg visited) ns)

danglingSwitch :: Var CFG -> Var CFGNode -> Var Bool
danglingSwitch cfg n  = let ss = filter' (not' ^. isCase) (_succs' cfg n) in followSuccessors cfg [] ss

analyze :: Var CFG -> [Var CFGNode]
analyze cfg  = let _ns = _nodes' cfg
                   switches = filter' isSwitch _ns in filter' (not' ^. (danglingSwitch cfg)) switches

-}