{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module CFG 
    ( module CFG
    , module NodeTypes
    ) where

import qualified Data.Text as T 
import qualified Data.List as L

import Language.C.Syntax.AST
import PresenceCondition
import qualified Data.Multimap as M
import GHC.Generics (Generic)
import Control.DeepSeq
import SPL
import NodeTypes
import Debug.Trace

--instance NFData NodeType where
--    rnf !n = seq n ()

data CFGNode = CFGNode {
    _nID     :: Int,
    _fname   :: T.Text,
    text    :: T.Text,
    ast     :: NodeType,
    _preds  :: [Int],
    __succs :: [Int]
    }
    deriving (Generic, NFData)


--instance NFData CFGNode where
--    rnf (CFGNode !id !t !a !ps !ss) = seq id (seq t (seq a (seq ps (seq ss ()))))

--getID (CFGNode i _ _ _ _) = i

data CFG = CFG {
    nodes :: M.ListMultimap Int CFGNode
    } 
    
instance NFData CFG where
    rnf n = (_nodes n) `seq` n `seq` ()

instance Show CFG where
    show cfg = show $ _nodes cfg

_find :: Int -> [Int] -> Bool
_find n _ns = --trace "find" $
        case _ns of
            []      -> False
            (h:_t)  -> if h == n then True else _find n _t
    
--uniquesOnly :: [Int] -> [Int]
--uniquesOnly _xs = 
--        case _xs of
--            [] -> []
--            (y : _ys) -> if _find y _ys then uniquesOnly _ys else y : (uniquesOnly _ys)
    
findNode cfg id = 
    let ns = (nodes cfg) M.! id
    in  --if null ns then trace (show id) ns else 
        ns

_succs :: CFG -> CFGNode -> [CFGNode]
_succs cfg n = 
    let rs  = map (findNode cfg) (__succs n)
        rs' =  filter (not . null) rs
    in  map head rs'
{-
vl2lv :: Var [a] -> [Var a]
vl2v vs = 
_succs' :: Var CFG -> Var CFGNode -> [Var CFGNode]
_succs' cfg n = 
    let ss = (_succs ^| ttPC) <*> cfg <*> n
-}

{-
_find :: Int -> [Int] -> Bool
_find n ns = --trace "find" $
    case ns of
        []      -> False
        (h:t)   -> if h == n then True else _find n t

uniquesOnly :: [Int] -> [Int]
uniquesOnly xs = 
    case xs of
        [] -> []
        (y : ys) -> if _find y ys then uniquesOnly ys else y : (uniquesOnly ys)
-}

_nodes :: CFG -> [CFGNode]
_nodes cfg = (snd . unzip . M.toList) $ nodes cfg

--_nodes' :: Var CFG -> [Var CFGNode]
--_nodes (Var cs) = assert (null cs) $ 

preds :: CFG -> CFGNode -> [CFGNode]
preds cfg n = map (head . ((nodes cfg) M.!)) (_preds n)

--_succs :: CFG -> CFGNode -> [CFGNode]
--_succs cfg n = map (head . ((_nodes cfg) M.!)) (uniquesOnly (__succs n))

instance Show CFGNode where
    show (CFGNode i fname t nt ps ss) =
        "Node: " ++ (show i) ++ "\t" ++ (show t) ++ "\t" ++ (show fname)
        ++ "\n\tAST: " ++ (show nt)
        ++ "\n\tpredecessors: " ++ (L.intercalate ", " (map show ps))
        ++ "\n\tsuccessors  : " ++ (L.intercalate ", " (map show ss)) ++ "\n"
