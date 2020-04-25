-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module CaseTermination where

import CFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace

find :: Int -> [Int] -> Bool
find n ns =
    case ns of
        []      -> False
        (h:t)   -> if h == n then True else find n t

isCase :: CFGNode -> Bool
isCase n = 
    case ast n of
        CFGStat (CCase _ _ _)   -> True
        _                       -> False

isDefault :: CFGNode -> Bool
isDefault n = 
    case ast n of
        CFGStat (CDefault _ _)  -> True
        _                       -> False

isBreak :: CFGNode -> Bool
isBreak n = 
    case ast n of
        CFGStat (CBreak _)      -> True
        _                       -> False

isFuncCall :: CFGNode -> Bool
isFuncCall n =
    case ast n of
        CFGDecl _   -> True
        _           -> False
{-
traverseNodes :: Maybe CFGNode -> [Int] -> [CFGNode] -> [CFGNode]
traverseNodes caseOnPath visited ns =
    concat (map (traverseFuncCFG caseOnPath visited) ns)

traverseFuncCFG :: Maybe CFGNode -> [Int] -> CFGNode -> [CFGNode]
traverseFuncCFG caseOnPath visited n = trace ((show n) ++ "\n\tCaseOnPath:" ++ (show caseOnPath)) $
    case n of
        (CFGNode i _ nt _ succ) -> 
            if      find i visited
            then    []
            else case nt of
                    CFGStat s -> case s of
                                    CCase _ _ _ -> if isJust caseOnPath
                                                   then (fromJust caseOnPath) : (traverseNodes (Just n) (i : visited) succ)
                                                   else traverseNodes (Just n) (i : visited) succ
                                    CDefault _ _-> if isJust caseOnPath
                                                   then (fromJust caseOnPath) : (traverseNodes Nothing (i : visited) succ)
                                                   else traverseNodes Nothing (i : visited) succ
                                    CBreak _    -> traverseNodes Nothing (i : visited) succ
                                    _           -> traverseNodes caseOnPath (i : visited) succ
                    _         -> traverseNodes caseOnPath (i : visited) succ
-}
followSuccessor :: CFG -> [Int] -> CFGNode -> Bool
followSuccessor cfg visited n = --trace (show n) $
    if      (find (nID n) visited) || (isBreak n) || (isFuncCall n)
    then    True
    else    if      (isCase n) || (isDefault n)
            then    False
            else    followSuccessors cfg ((nID n) : visited) (succs cfg n)
 
followSuccessors :: CFG -> [Int] -> [CFGNode] -> Bool
followSuccessors cfg visited ns =  
    foldr (&&) True $ map (followSuccessor cfg visited) ns

terminatedCase :: CFG -> CFGNode -> Bool
terminatedCase cfg n = --trace (show n) $
    let ss = filter (not . isCase) $ (succs cfg n)
    in  followSuccessors cfg [] ss

analyze :: CFG -> [CFGNode]
analyze cfg = --trace (show ns) $
    let ns = nodes cfg 
        cases = filter isCase ns 
    in  filter (not . (terminatedCase cfg)) cases
