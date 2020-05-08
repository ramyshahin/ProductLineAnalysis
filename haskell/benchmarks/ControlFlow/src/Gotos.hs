-- Return Average code analysis
-- Returning the average number of goto statements per label
module Gotos where

import CFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace
import Data.Ratio

find :: Int -> [Int] -> Bool
find n _ns =
    case _ns of
        []      -> False
        (h:_t)  -> if h == n then True else find n _t

isFnRoot :: CFGNode -> Bool
isFnRoot n =
    case ast n of
       CFGFuncRoot _ -> True
       --CFGFunc _     -> True
       _             -> False

isReturn :: CFGNode -> Bool
isReturn n = 
    case ast n of
        CFGStat (CReturn _ _) -> True
        _                   -> False
        
isGoto :: CFGNode -> Bool
isGoto n = 
            case ast n of
                CFGStat (CGoto _ _) -> True
                _                   -> False

isLabel :: CFGNode -> Bool
isLabel n = 
                    case ast n of
                        CFGStat (CLabel _ _ _ _) -> True
                        _                   -> False

isFuncCall :: CFGNode -> Bool
isFuncCall n =
    case ast n of
        CFGDecl _           -> True
        CFGStat (CBreak _)  -> True
        CFGFuncRoot _       -> True
        _                   -> False

followSuccessor :: CFG -> [Int] -> CFGNode -> Integer
followSuccessor cfg _visited n = --trace (show n) $
    if      (find (_nID n) _visited) || (isFuncCall n)
    then    0
    else    if      isReturn n
            then    1
            else    followSuccessors cfg ((_nID n) : _visited) n

followSuccessors :: CFG -> [Int] -> CFGNode -> Integer
followSuccessors cfg _visited n = 
    let _ss = _succs cfg n
    in  foldr (\a b -> a + b) 0 (map (followSuccessor cfg _visited) _ss)

returnAvg :: CFG -> CFGNode -> Integer
returnAvg cfg n = --trace (show n) $
    followSuccessors cfg [_nID n] n

analyze :: CFG -> Rational
analyze cfg = --trace (show ns) $
    let _ns = _nodes cfg 
        _ls = filter isLabel _ns
        _gs = filter isGoto _ns
        labelCount = length _ls
        gotoCount  = length _gs
    in  if labelCount == 0 then 0 else (toInteger gotoCount) % (toInteger labelCount)
