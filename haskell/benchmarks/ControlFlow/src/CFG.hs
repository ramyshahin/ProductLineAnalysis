module CFG where

import qualified Data.Text as T 
import qualified Data.List as L

import Language.C.Syntax.AST
import PresenceCondition
import qualified Data.MultiMap as M

data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGFuncRoot T.Text
  | CFGDummy    T.Text
  deriving Show

data CFGNode = CFGNode {
    _nID     :: Int,
    text    :: T.Text,
    ast     :: NodeType,
    _preds  :: [Int],
    __succs :: [Int]
    }

getID (CFGNode i _ _ _ _) = i

data CFG = CFG {
    _nodes :: M.MultiMap Int CFGNode
} 

instance Show CFG where
    show cfg = show $ nodes cfg

nodes :: CFG -> [CFGNode]
nodes cfg = (snd . unzip . M.toList) $ _nodes cfg

preds :: CFG -> CFGNode -> [CFGNode]
preds cfg n = map (head . ((_nodes cfg) M.!)) (_preds n)

_succs :: CFG -> CFGNode -> [CFGNode]
_succs cfg n = map (head . ((_nodes cfg) M.!)) (__succs n)

instance Show CFGNode where
    show (CFGNode i t nt ps ss) =
        "Node: " ++ (show i) ++ "\t" ++ (show t) 
        ++ "\n\tAST: " ++ (show nt)
        ++ "\n\tpredecessors: " ++ (L.intercalate ", " (map show ps))
        ++ "\n\tsuccessors  : " ++ (L.intercalate ", " (map show ss)) ++ "\n"
