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
                 split1 __dummy__ = case __dummy__ of (h:t) -> (h, t) 
                 ret = liftedCase (ns) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                        (h:t) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]
             in --trace ("find: n:  " ++ (show n)) $ 
                --trace ("find: ns: " ++ (show ns)) $
                --trace ("find: ret:" ++ (show ret)) $
                ret

isCase :: Var CFGNode -> Var Bool
isCase n  = let case0 = (mkVarT True)
                split0 __dummy__ = case __dummy__ of CFGStat (CCase _ _ _) -> ()
                case1 = (mkVarT False)
                split1 __dummy__ = case __dummy__ of _ -> () 
                ret = liftedCase ((mkVarT ast) <*> n) (\__dummy__ -> case __dummy__ of CFGStat (CCase _ _ _) -> 0
                                                                                       _ -> 1) [(uncurry0 case0) . (liftV split0), (uncurry0 case1) . (liftV split1)]
            in  --trace ("isCase-in:  " ++ (show n)) $ 
                --trace ("isCase-out: " ++ (show ret)) $
                ret

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
followSuccessor cfg visited n  = 
    let v = find ((mkVarT nID) <*> n) visited
        b = isBreak n
        f = isFuncCall n
        c = v ^|| b ^|| f
        ret = liftedCond (c) 
                         ((mkVarT True)) 
                         (liftedCond ((isCase n) ^|| (isDefault n)) 
                                     ((mkVarT False)) 
                                     (followSuccessors cfg (((mkVarT nID) <*> n) ^: visited) ((mkVarT succs) <*> cfg <*> n)))
    in  trace ("followSuccessor") $
        --trace ("\tinput: " ++ (show n)) $
        --trace ("\tfollowSuccessor visited: " ++ (show v)) $
        --trace ("\tfollowSuccessor isBreak: " ++ (show b)) $
        --trace ("\tfollowSuccessor isFuncCall: " ++ (show f)) $
        --trace ("\tfollowSuccessor condition: " ++ (show c)) $
        ret

followSuccessors :: Var CFG -> Var [Int] -> Var [CFGNode] -> Var Bool
followSuccessors cfg visited ns  = 
    let ret = foldr' (\a b -> a ^&& b) (mkVarT True) (map' (followSuccessor cfg visited) ns)
    in trace ("followSuccessors") ret

terminatedCase :: Var CFG -> Var CFGNode -> Var Bool
terminatedCase cfg n  = let ss = filter' (not' ^. isCase) ((mkVarT succs) <*> cfg <*> n) 
                            ret = followSuccessors cfg (mkVarT []) ss
                        in  trace ("terminatedCase") ret

analyze :: Var CFG -> Var [CFGNode]
analyze cfg  = let ns = (mkVarT nodes) <*> cfg
                   cases = filter' isCase ns 
                   ret = filter' (not' ^. (terminatedCase cfg)) cases
                in trace ("cases: " ++ (show cases)) ret
