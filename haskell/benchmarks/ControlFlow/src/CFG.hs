module CFG where

import qualified Data.Text.Lazy as T 
import Language.C.Syntax.AST
import PresenceCondition
import qualified Data.Map as M

data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGDummy    T.Text
  deriving Show

data CFGNode = CFGNode {
    nID     :: Int,
    text    :: T.Text,
    ast     :: NodeType,
    _preds  :: [Int],
    _succs  :: [Int]
    }

getID (CFGNode i _ _ _ _) = i

data CFG = CFG {
    _nodes :: M.Map Int CFGNode
} 

nodes :: CFG -> [CFGNode]
nodes cfg = M.elems $ _nodes cfg

preds :: CFG -> CFGNode -> [CFGNode]
preds cfg n = map ((_nodes cfg) M.!) (_preds n)

succs :: CFG -> CFGNode -> [CFGNode]
succs cfg n = map ((_nodes cfg) M.!) (_succs n)

instance Show CFGNode where
    show (CFGNode i t nt ps ss) =
        "Node: " ++ (show i) ++ "\t" ++ (show t) 
        ++ "\n\tAST: " ++ (show nt)
        ++ "\n\tpredecessors: " ++ (foldr (\l r -> l ++ ", " ++ r) "" (map show ps))
        ++ "\n\tsuccessors  : " ++ (foldr (\l r -> l ++ ", " ++ r) "" (map show ss))
