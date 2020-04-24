-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module CaseTermination where

import CFG
import Language.C.Syntax.AST
import Data.Maybe

find :: Int -> [Int] -> Bool
find n ns =
    case ns of
        []      -> False
        (h:t)   -> if h == n then True else find n t

traverseNodes :: Maybe CFGNode -> [Int] -> [CFGNode] -> [CFGNode]
traverseNodes caseOnPath visited ns =
    concat (map (traverseFuncCFG caseOnPath visited) ns)

traverseFuncCFG :: Maybe CFGNode -> [Int] -> CFGNode -> [CFGNode]
traverseFuncCFG caseOnPath visited n =
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
                    
analyze :: [CFGNode] -> [CFGNode]
analyze ns = traverseNodes Nothing [] ns
