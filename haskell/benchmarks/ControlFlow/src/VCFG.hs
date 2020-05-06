{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module VCFG 
    ( module VCFG
    , module NodeTypes
    ) where

import Control.Exception
import Language.C.Syntax.AST
import qualified CFG as C
import qualified Data.Text as T
import qualified Data.MultiMap as M
import GHC.Generics (Generic)
import Control.DeepSeq
import SPL
import PresenceCondition
import Debug.Trace
import NodeTypes

lv2vl :: [Var a] -> Var [a]
lv2vl = foldr (|:|) (mkVarT [])

{-
data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGFuncRoot T.Text
  | CFGDummy    T.Text
    deriving (Show, Generic, NFData)
-}

data CFGNode = CFGNode {
    _nID :: Int,
    text :: T.Text,
    ast :: C.NodeType,
    _preds :: [Var Int],
    __succs :: [Var Int]
    } deriving (Show, Generic, NFData)

toShallowNode :: (CFGNode, PresenceCondition) -> Var C.CFGNode
toShallowNode (n, pc) = 
    let ps = lv2vl $ _preds n
        ss = lv2vl $ __succs n
    in  (C.CFGNode ^| pc) <*> ((_nID n) ^| pc) <*> ((text n) ^| pc) 
                            <*> ((ast n) ^| pc) <*> ps <*> ss

data CFG = CFG {
    nodes :: M.MultiMap Int (CFGNode, PresenceCondition)
}

instance NFData CFG where
    rnf n = (_nodes n) `seq` n `seq` ()

_nodes :: CFG -> [(CFGNode, PresenceCondition)]
_nodes cfg = (snd . unzip . M.toList) $ nodes cfg

mkShallowCFG :: [C.CFGNode] -> C.CFG
mkShallowCFG  ns = C.CFG $ foldr (\n m -> M.insert (C._nID n) n m) M.empty ns

mkShallowCFG' = liftV mkShallowCFG

toShallowCFG :: CFG -> Var C.CFG
toShallowCFG c =
    let ns = _nodes c
        ns' = map toShallowNode ns
    in  mkShallowCFG' (lv2vl ns')

_succs' :: Var CFG -> Var CFGNode -> [Var CFGNode]
_succs' (Var ((cfg, pc) : ss)) n'@(Var n) = 
    --trace (show n') $
    assert (null ss) $
    --trace (show pc) $
    --assert (pc == ttPC) $ 
    let ss' = foldr (++) [] $ map (\(n',_) -> if (_nID n') == 0 then [] else __succs n') n
    in  map (\(Var xs) -> Var $ map 
        (\(n,pc) -> let ys = --trace (show n) $ 
                            (nodes cfg) M.! n
                    in  assert (not (null ys)) $ head ys) 
            (filter (\(n, pc) -> n /= 0) xs)) 
        ss'
                    
_succs' (Var []) _ = []

dummyNode = CFGNode 0 T.empty (C.CFGDummy T.empty) [] []

--node2Vnode :: CFGNode -> Var CFGNode
--node2Vnode n = 
--    let (Var ids) = _nID n
--    in  Var $ map (\(id, pc) -> if id == 0 then (dummyNode, pc) else (n, pc)) ids

mkV :: a -> (a, PresenceCondition) -> Var a
mkV dummy (v, pc) = 
    if   pc == ttPC
    then mkVarT v
    else mkVars [(v, pc), (dummy, negPC pc)]

_nodes' :: Var CFG -> [Var CFGNode]
_nodes' (Var ((cfg, pc) : ss)) = 
    assert (null ss) $
    assert (pc == ttPC) $
    let ns = (snd . unzip . M.toList) $ nodes cfg
    in  map (\v -> mkV dummyNode v) ns 

_nID' :: Var CFGNode -> Var Int
_nID' (Var n) = foldr union (Var []) $ map (\(n', pc) -> (_nID n') ^| pc) n