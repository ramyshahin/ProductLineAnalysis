{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module VCFG 
    ( module VCFG
    , module NodeTypes
    ) where

import Control.Exception
import Language.C.Syntax.AST
import qualified CFG as C
import qualified Data.Text as T
import qualified Data.Multimap as M
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
    _fname :: T.Text,
    text :: T.Text,
    ast :: C.NodeType,
    _preds :: [Var Int],
    __succs :: [Var Int]
    } deriving (Show, Generic, NFData)

dummyCNode = C.CFGNode 0 (T.pack "") (T.pack "") (CFGDummy (T.pack "")) [] []

toShallowNode :: (CFGNode, PresenceCondition) -> Var C.CFGNode
toShallowNode (n, pc) = 
    let ps = lv2vl $ _preds n
        ss = lv2vl $ __succs n
        d  = (C.CFGNode ^| pc) <*> ((_nID n) ^| pc) <*> ((_fname n) ^| pc) <*> ((text n) ^| pc) 
                               <*> ((ast n) ^| pc) <*> ps <*> ss
    in  fixCompleteness dummyCNode d

data CFG = CFG {
    nodes :: M.ListMultimap Int (CFGNode, PresenceCondition)
}

instance NFData CFG where
    rnf n = n `seq` ((M.toList . nodes) n) `seq` ()

_nodes :: CFG -> [(CFGNode, PresenceCondition)]
_nodes cfg = (snd . unzip . M.toList) $ nodes cfg

mkShallowCFG :: [C.CFGNode] -> C.CFG
mkShallowCFG  ns = C.CFG $! foldr (\n m -> M.append (C._nID n) n m) M.empty ns

mkShallowCFG' ns@(Var ns') = --trace ("Variants: " ++ (show (length ns'))) $ 
    (liftV mkShallowCFG) ns

toShallowCFG :: CFG -> Var C.CFG
toShallowCFG c =
    let !ns = _nodes c
        !ns' = map toShallowNode ns
        !vl@(Var vl') = lv2vl ns'
        !ret = mkShallowCFG' vl
    in  --trace ("Var Node count: " ++ (show (length vl'))) $
        ret

_succs' :: Var CFG -> Var CFGNode -> [Var CFGNode]
_succs' (Var ((cfg, pc) : ss)) n'@(Var n) = 
    assert (null ss) $
    let ss' = foldr (++) [] $ map (\(n',_) -> if (_nID n') == 0 then [] else __succs n') n
        zs  = map (\(Var xs) -> Var $ 
                                  map 
                                    (\(n,pc) -> let ys = --trace (show n) $ 
                                                        (nodes cfg) M.! n
                                                in  assert (not (null ys)) $ head ys)
                                    (filter (\(n, pc) -> n /= 0) xs)
                  ) ss'
    in  --zs 
        map (fixCompleteness dummyNode) zs
_succs' (Var []) _ = []

{-
fixCompleteness :: Var a -> Var a
fixCompleteness v = 
    if      definedAt v == ttPC 
    then    v
    else    SPL.union v (dummyNode ^| (undefinedAt v))
-}

dummyNode = CFGNode 0 T.empty T.empty (C.CFGDummy T.empty) [] []

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
    in  --map (\x -> Var [x]) ns
        map (\v -> mkV dummyNode v) ns

_nID' :: Var CFGNode -> Var Int
_nID' (Var n) = foldr union (Var []) $ map (\(n', pc) -> (_nID n') ^| pc) n
