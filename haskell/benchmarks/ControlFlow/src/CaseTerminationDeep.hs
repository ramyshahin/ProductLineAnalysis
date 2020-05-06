-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module CaseTerminationDeep where

import SPL
import VCFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace
import Control.Exception
--import qualified Data.MultiMap as M

find :: Var Int -> [Var Int] -> Var Bool
find n _ns  = --trace "find" $
            case _ns of [] -> (False ^| ttPC)
                        (h:_t) -> liftedCond (((==) ^| ttPC) <*> (h) <*> (n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) _t)

isCase :: Var CFGNode -> Var Bool
isCase n  = --trace "isCase" $
            let case0 __cntxt__ = (True ^| __cntxt__)
                split0 __dummy__ = case __dummy__ of CFGStat (CCase _ _ _) -> ()
                case1 __cntxt__ = (False ^| __cntxt__)
                split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CCase _ _ _) -> 0
                                                                                                                                  _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isDefault :: Var CFGNode -> Var Bool
isDefault n  = --trace "isDefault" $
               let case0 __cntxt__ = (True ^| __cntxt__)
                   split0 __dummy__ = case __dummy__ of CFGStat (CDefault _ _) -> ()
                   case1 __cntxt__ = (False ^| __cntxt__)
                   split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CDefault _ _) -> 0
                                                                                                                                     _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isBreak :: Var CFGNode -> Var Bool
isBreak n  = --trace "isBreak" $
             let case0 __cntxt__ = (True ^| __cntxt__)
                 split0 __dummy__ = case __dummy__ of CFGStat (CBreak _) -> ()
                 case1 __cntxt__ = (False ^| __cntxt__)
                 split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CBreak _) -> 0
                                                                                                                                   _ -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n  = --trace "isFuncCall" $
                let case0 __cntxt__ = (True ^| __cntxt__)
                    split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                    case1 __cntxt__ = (True ^| __cntxt__)
                    split1 __dummy__ = case __dummy__ of CFGFunc _ -> ()
                    case2 __cntxt__ = (False ^| __cntxt__)
                    split2 __dummy__ = case __dummy__ of _ -> () in liftedCase ((ast ^| ttPC) <*> n) (\__dummy__ -> case __dummy__ of CFGDecl _ -> 0
                                                                                                                                      CFGFunc _ -> 1
                                                                                                                                      _ -> 2) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

followSuccessor :: Var CFG -> [Var Int] -> Var CFGNode -> Var Bool
followSuccessor cfg _visited n  = --trace "followSuccessor" $
    let ret = liftedCond (((||) ^| ttPC) <*> ((find (_nID' n) _visited)) <*> (((||) ^| ttPC) <*> ((isBreak n)) <*> ((isFuncCall n)))) 
                         (\__cntxt__ -> --trace ("T: " ++ (show __cntxt__)) 
                            (True ^| __cntxt__)) 
                         (\__cntxt__ -> --trace ("F: " ++ (show __cntxt__)) $
                             liftedCond (((||) ^| __cntxt__) <*> ((isCase (n /^ __cntxt__))) <*> ((isDefault (n /^ __cntxt__)))) 
                                        (\__cntxt__ -> --trace ("FT: " ++ (show __cntxt__)) 
                                            (False ^| __cntxt__)) 
                                        (\__cntxt__ -> --trace ("FF: " ++ (show __cntxt__)) $ 
                                            followSuccessors (cfg /^ __cntxt__) ((_nID' (n /^ __cntxt__)) ^: _visited) (_succs' (cfg /^ __cntxt__) (n /^ __cntxt__)))
                         )
    in  assert (disjInv ret) $ ret

followSuccessors :: Var CFG -> [Var Int] -> [Var CFGNode] -> Var Bool
followSuccessors cfg visited ns  = --trace "followSuccessors" $
    --trace (show visited) $
    --trace (show ns) $
    foldr' (\a b -> 
                --trace ("a: " ++ (show a)) $
                --trace ("b: " ++ (show b)) $
                --trace ("ttPC:" ++ (show ttPC)) $ 
                --trace ("ffPC:" ++ (show ffPC)) $ 
                ((&&) ^| ttPC) <*> (a) <*> (b)) 
           (True ^| ttPC) 
           (map' (followSuccessor cfg visited) ns)

terminatedCase :: Var CFG -> Var CFGNode -> Var Bool
terminatedCase cfg n  = --trace "terminatedCase" $
    let ss = filter' (not' ^. isCase) (_succs' cfg n) in followSuccessors cfg [] ss

analyze :: Var CFG -> [Var CFGNode]
analyze cfg  = --trace "analyze" $
               let _ns = _nodes' cfg
                   cases = filter' isCase _ns in filter' (not' ^. (terminatedCase cfg)) cases

