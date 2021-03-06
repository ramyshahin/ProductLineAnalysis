-- Call density code analysis
-- Returning the average number of function calls per function
module CallDensity where

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
        
isFuncCall :: CFGNode -> Bool
isFuncCall n =
    case ast n of
        CFGDecl _           -> True
        CFGStat (CBreak _)  -> True
        CFGFuncRoot _       -> True
        _                   -> False

followSuccessor :: CFG -> [Int] -> CFGNode -> Integer
followSuccessor cfg _visited n = --trace (show n) $
    if      (find (_nID n) _visited)
    then    0
    else    if      isFuncCall n
            then    1
            else    followSuccessors cfg ((_nID n) : _visited) n

followSuccessors :: CFG -> [Int] -> CFGNode -> Integer
followSuccessors cfg _visited n = 
    let _ss = _succs cfg n
    in  foldr (\a b -> a + b) 0 (map (followSuccessor cfg _visited) _ss)

callDensity :: CFG -> CFGNode -> Integer
callDensity cfg n = --trace (show n) $
    followSuccessors cfg [_nID n] n

analyze :: CFG -> Rational
analyze cfg = --trace (show ns) $
    let _ns = _nodes cfg 
        _fns = --trace (show _ns) $ 
            filter isFnRoot _ns 
        fnCount = length _fns
        total   = foldr (\a b -> a + b) 0 (map (callDensity cfg) _fns)
    in  total % (toInteger fnCount)
