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
find n ns  = let case0 = (mkVarT False)
                 split0 __dummy__ = case __dummy__ of [] -> ()
                 case1 h t = liftedCond (h ^== n) ((mkVarT True)) (find n t)
                 split1 __dummy__ = case __dummy__ of (h:t) -> (h, t) in liftedCase (ns) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                          (h:t) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]

isCase :: Var CFGNode -> Var Bool
isCase n  = let case0 = (mkVarT True)
                split0 __dummy__ = case __dummy__ of CFGStat (CCase _ _ _) -> ()
                case1 = (mkVarT False)
                split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((mkVarT ast) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CCase _ _ _) -> 0
                                                                                                                                 _ -> 1) [(uncurry0 case0) . (liftV split0), (uncurry0 case1) . (liftV split1)]

isDefault :: Var CFGNode -> Var Bool
isDefault n  = let case0 = (mkVarT True)
                   split0 __dummy__ = case __dummy__ of CFGStat (CDefault _ _) -> ()
                   case1 = (mkVarT False)
                   split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((mkVarT ast) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CDefault _ _) -> 0
                                                                                                                                    _ -> 1) [(uncurry0 case0) . (liftV split0), (uncurry0 case1) . (liftV split1)]

isBreak :: Var CFGNode -> Var Bool
isBreak n  = let case0 = (mkVarT True)
                 split0 __dummy__ = case __dummy__ of CFGStat (CBreak _) -> ()
                 case1 = (mkVarT False)
                 split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((mkVarT ast) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CBreak _) -> 0
                                                                                                                                  _ -> 1) [(uncurry0 case0) . (liftV split0), (uncurry0 case1) . (liftV split1)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n  = let case0 = (mkVarT True)
                    split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
                    case1 = (mkVarT False)
                    split1 __dummy__ = case __dummy__ of _ -> () in liftedCase ((mkVarT ast) <*> n) (\__dummy__ -> case __dummy__ of CFGDecl _ -> 0
                                                                                                                                     _ -> 1) [(uncurry0 case0) . (liftV split0), (uncurry0 case1) . (liftV split1)]

followSuccessor :: Var CFG -> Var [Int] -> Var CFGNode -> Var Bool
followSuccessor cfg visited n  = liftedCond ((find ((mkVarT nID) <*> n) visited) ^|| (isBreak n) ^|| (isFuncCall n)) ((mkVarT True)) (liftedCond ((isCase n) ^|| (isDefault n)) ((mkVarT False)) (followSuccessors cfg (((mkVarT nID) <*> n) ^: visited) ((mkVarT succs) <*> cfg <*> n)))
 
followSuccessors :: Var CFG -> Var [Int] -> Var [CFGNode] -> Var Bool
followSuccessors cfg visited ns  = foldr' (\a b -> a ^&& b) (mkVarT True) (map' (followSuccessor cfg visited) ns)

terminatedCase :: Var CFG -> Var CFGNode -> Var Bool
terminatedCase cfg n  = let ss = filter' (not' ^. isCase) ((mkVarT succs) <*> cfg <*> n) in followSuccessors cfg (mkVarT []) ss

analyze :: Var CFG -> Var [CFGNode]
analyze cfg  = let cases = filter' isCase ns
                   ns = (mkVarT nodes) <*> cfg in filter' (not' ^. (terminatedCase cfg)) cases

