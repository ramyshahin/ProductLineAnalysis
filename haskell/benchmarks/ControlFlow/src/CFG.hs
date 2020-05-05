{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module CFG where

import qualified Data.Text as T 
import qualified Data.List as L

import Language.C.Syntax.AST
import PresenceCondition
import qualified Data.MultiMap as M
import GHC.Generics (Generic)
import Control.DeepSeq

data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGFuncRoot T.Text
  | CFGDummy    T.Text
    deriving (Show, Generic, NFData)

--instance NFData NodeType where
--    rnf !n = seq n ()

data CFGNode = CFGNode {
    _nID     :: Int,
    text    :: T.Text,
    ast     :: NodeType,
    _preds  :: [Int],
    __succs :: [Int]
    }
    deriving (Generic, NFData)

--instance NFData CFGNode where
--    rnf (CFGNode !id !t !a !ps !ss) = seq id (seq t (seq a (seq ps (seq ss ()))))

getID (CFGNode i _ _ _ _) = i

data CFG = CFG {
    _nodes :: M.MultiMap Int CFGNode
    } 
    
instance NFData CFG where
    rnf n = seq n ()

instance Show CFG where
    show cfg = show $ nodes cfg

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

nodes :: CFG -> [CFGNode]
nodes cfg = (snd . unzip . M.toList) $ _nodes cfg

preds :: CFG -> CFGNode -> [CFGNode]
preds cfg n = map (head . ((_nodes cfg) M.!)) (_preds n)

_succs :: CFG -> CFGNode -> [CFGNode]
_succs cfg n = map (head . ((_nodes cfg) M.!)) (uniquesOnly (__succs n))

instance Show CFGNode where
    show (CFGNode i t nt ps ss) =
        "Node: " ++ (show i) ++ "\t" ++ (show t) 
        ++ "\n\tAST: " ++ (show nt)
        ++ "\n\tpredecessors: " ++ (L.intercalate ", " (map show ps))
        ++ "\n\tsuccessors  : " ++ (L.intercalate ", " (map show ss)) ++ "\n"
