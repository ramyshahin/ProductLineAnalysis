-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Checking whether a non-void function has a return statement
module Return where

import CFG
import Language.C.Syntax.AST
import Data.Maybe
import Debug.Trace

find :: Int -> [Int] -> Bool
find n _ns =
    case _ns of
        []      -> False
        (h:_t)  -> if h == n then True else find n _t

{-
isCase :: CFGNode -> Bool
isCase n = 
    case ast n of
        CFGStat (CCase _ _ _)   -> True
        _                       -> False
-}

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
        
isFuncCall :: CFGNode -> Bool
isFuncCall n =
    case ast n of
        CFGDecl _     -> True
        CFGFuncRoot _ -> True
        _             -> False

followSuccessor :: CFG -> [Int] -> CFGNode -> Bool
followSuccessor cfg _visited n = --trace (show n) $
    if      (find (_nID n) _visited) || (isFuncCall n)
    then    False
    else    if      (isReturn n)
            then    True
            else    followSuccessors cfg ((_nID n) : _visited) (_succs cfg n)

followSuccessors :: CFG -> [Int] -> [CFGNode] -> Bool
followSuccessors cfg _visited _ns =  
    foldr (\a b -> a || b) False (map (followSuccessor cfg _visited) _ns)

hasReturn :: CFG -> CFGNode -> Bool
hasReturn cfg n = --trace (show n) $
    followSuccessors cfg [] [n]

analyze :: CFG -> [CFGNode]
analyze cfg = --trace (show ns) $
    let _ns = _nodes cfg 
        fns = filter isFnRoot _ns 
    in  filter (not . (hasReturn cfg)) fns
