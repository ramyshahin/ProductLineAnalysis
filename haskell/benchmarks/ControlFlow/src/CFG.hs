module CFG where

import qualified Data.Text.Lazy as T 
import Language.C.Syntax.AST
import PresenceCondition
import Data.Hashable

data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGDummy    T.Text
  deriving Show

data CFGNode = CFGNode Int T.Text NodeType [CFGNode] [CFGNode]

getID (CFGNode i _ _ _ _) = i

instance Show CFGNode where
    show (CFGNode i t nt ps ss) =
        "Node: " ++ (show i) ++ "\t" ++ (show nt) ++ 
        "\n\tpredecessors: " ++ (foldr (\l r -> l ++ ", " ++ r) "" (map (show . getID) ps)) ++
        "\n\tsuccessors  : " ++ (foldr (\l r -> l ++ ", " ++ r) "" (map (show . getID) ss))

instance Eq CFGNode where
    (==) n0 n1 = (getID n0 == getID n1)

instance Hashable CFGNode where
    hashWithSalt s n = hashWithSalt s (getID n)