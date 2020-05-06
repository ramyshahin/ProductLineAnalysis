-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module CaseTermination where

import CFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace
import qualified Data.MultiMap as M

find :: Int -> [Int] -> Bool
find n _ns = --trace "find" $
    case _ns of
        []      -> False
        (h:_t)   -> if h == n then True else find n _t

isCase :: CFGNode -> Bool
isCase n = --trace "isCase" $
    case ast n of
        CFGStat (CCase _ _ _)   -> True
        _                       -> False

isDefault :: CFGNode -> Bool
isDefault n = --trace "isDefault" $
    case ast n of
        CFGStat (CDefault _ _)  -> True
        _                       -> False

isBreak :: CFGNode -> Bool
isBreak n = --trace "isBreak" $
    case ast n of
        CFGStat (CBreak _)      -> True
        _                       -> False

isFuncCall :: CFGNode -> Bool
isFuncCall n = --trace "isFuncCall" $
    case ast n of
        CFGDecl _   -> True
        CFGFunc _   -> True
        _           -> False

followSuccessor :: CFG -> [Int] -> CFGNode -> Bool
followSuccessor cfg _visited n = --trace (show n) $
    if      (find (_nID n) _visited) || (isBreak n) || (isFuncCall n)
    then    True
    else    if      (isCase n) || (isDefault n)
            then    False
            else    followSuccessors cfg ((_nID n) : _visited) (_succs cfg n)

followSuccessors :: CFG -> [Int] -> [CFGNode] -> Bool
followSuccessors cfg visited ns = --trace "followSuccessors" $
    foldr (\a b -> a && b) True (map (followSuccessor cfg visited) ns)

terminatedCase :: CFG -> CFGNode -> Bool
terminatedCase cfg n = --trace "terminatedCase" $ -- (show n) $
    let ss = filter (not . isCase) (_succs cfg n)
    in  followSuccessors cfg [] ss

analyze :: CFG -> [CFGNode]
analyze cfg = --trace (show ns) $
    let _ns = _nodes cfg 
        cases = filter isCase _ns 
    in  filter (not . (terminatedCase cfg)) cases
