
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}

module VCFG 
--    ( module VCFG
--    , module NodeTypes
--    ) 
    where
import Control.Exception
import Control.Applicative
import Language.C.Syntax.AST
import CFG
import qualified Data.Text as T
import qualified Data.Multimap as M
import GHC.Generics (Generic)
import Control.DeepSeq
import SPL
import PresenceCondition
import Debug.Trace
import NodeTypes

{-# INLINE (|:|) #-}
(|:|) :: V a -> V [a] -> V [a]
(|:|) = liftA2 (:)
infixr 5 |:|

lv2vl :: [V a] -> V [a]
lv2vl = foldr (|:|) (v [])
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


data CFGNode = CFGNode {
    _nID :: Int,
    _fname :: T.Text,
    text :: T.Text,
    ast :: C.NodeType,
    _preds :: [V Int],
    __succs :: [V Int]
    } deriving (Show
    --, Generic
    )
-}

-- dummyCNode = C.CFGNode 0 (T.pack "") (T.pack "") (CFGDummy (T.pack "")) [] []

toShallowNode :: (CFGNode, PresenceCondition) -> V CFGNode
toShallowNode (n, pc) = 
    let --ps = lv2vl $ _preds n
        --ss = lv2vl $ __succs n
        d  = (CFGNode ^| pc) <*> ((_nID n) ^| pc) <*> ((_fname n) ^| pc) <*> ((text n) ^| pc) 
                               <*> ((ast n) ^| pc)
    in  fixCompleteness d

toShallowEdge :: (CFGEdge, PresenceCondition) -> V CFGEdge
toShallowEdge ((CFGEdge f t), pc) = 
    let d  = (CFGEdge ^| pc) <*> (f ^| pc) <*> (t ^| pc) 
    in  fixCompleteness d

{-
data CFG = CFG {
    nodes :: M.ListMultimap Int (CFGNode, PresenceCondition)
}

instance NFData CFG where
    rnf n = n `seq` ((M.toList . nodes) n) `seq` ()

_nodes :: CFG -> [(CFGNode, PresenceCondition)]
_nodes cfg = (snd . unzip . M.toList) $ nodes cfg
-}

mkShallowCFG :: [CFGNode] -> [CFGEdge] -> CFG
mkShallowCFG ns es = CFG ns es 

mkShallowCFG' ns es = --trace ("Variants: " ++ (show (length ns'))) $ 
    (v mkShallowCFG) <*> ns <*> es


toShallowCFG :: CFG -> V CFG
toShallowCFG (CFG ns es) =
    let ns' = map toShallowNode (map (\n -> (n, allConfigs)) ns)
        es' = map toShallowEdge (map (\e -> (e, allConfigs)) es)
        vl  = lv2vl ns'
        ve  = lv2vl es' 
        ret = mkShallowCFG' vl ve
    in  --trace ("V Node count: " ++ (show (length vl'))) $
        ret

{-
_succs' :: V CFG -> V CFGNode -> [V CFGNode]
_succs' vs n = 
    case vs of 
        ((cfg, pc) : ss) -> 
            assert (null ss) $
            let ss' = foldr (++) [] $ map (\(n',_) -> if (_nID n') == 0 then [] else __succs n') n
                zs  = map (\xs -> V $ 
                                  map 
                                    (\(n,pc) -> let ys = --trace (show n) $ 
                                                        (nodes cfg) M.! n
                                                in  assert (not (null ys)) $ head ys)
                                    (filter (\(n, pc) -> n /= 0) xs)
                      ) ss'
            in  --zs 
                map (fixCompleteness dummyNode) zs 
        [] -> []
-}

fixCompleteness :: V a -> V a
fixCompleteness v = v
{- 
    if      definedAt v == allConfigs 
    then    v
    else    SPL.union v (dummyNode ^| (undefinedAt v))
-}
--dummyNode = CFGNode 0 T.empty T.empty (C.CFGDummy T.empty) --[] []

--node2Vnode :: CFGNode -> V CFGNode
--node2Vnode n = 
--    let (V ids) = _nID n
--    in  V $ map (\(id, pc) -> if id == 0 then (dummyNode, pc) else (n, pc)) ids

mkV :: a -> (a, PresenceCondition) -> V a
mkV dummy (x, pc) = 
    if   pc == allConfigs
    then v x
    else mkVars [(x, pc), (dummy, negPC pc)]

{-
_nodes' :: V CFG -> [V CFGNode]
_nodes' vs = 
    case vs of 
        V ((cfg, pc) : ss) -> 
            assert (null ss) $ assert (pc == allConfigs) $
            let ns = (snd . unzip . M.toList) $ nodes cfg
            in  --map (\x -> V [x]) ns 
                map (\v -> mkV dummyNode v) ns
-}

--_nID' :: V CFGNode -> V Int
--_nID' (V n) = foldr union emptyV $ map (\(n', pc) -> (_nID n') ^| pc) n