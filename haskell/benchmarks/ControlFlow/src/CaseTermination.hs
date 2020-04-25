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
followSuccessor :: [Int] -> CFGNode -> Bool
followSuccessor visited n =
    if      (find (nodeID n) visited) || (isBreak n) || (isFuncCall n)
    then    True
    else    if      (isCase n) || (isDefault n)
            then    False
            else    followSuccessors ((nodeID n) : visited) (succs n)
 
followSuccessors :: [Int] -> [CFGNode] -> Bool
followSuccessors visited ns =
    foldr (&&) True $ map (followSuccessor visited) ns

terminatedCase :: CFGNode -> Bool
terminatedCase n = 
    let ss = filter (not . isCase) $ succs n
    in  followSuccessors [] ss

analyze :: [CFGNode] -> [CFGNode]
analyze ns = 
    let cases = filter isCase ns 
    in  filter (not . terminatedCase) cases
