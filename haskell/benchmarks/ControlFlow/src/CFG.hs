module CFG where

import Data.Text.Lazy
import Language.C.Syntax.AST
import PresenceCondition

data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     Text
  | CFGFunc     Text
  | CFGDummy    Text
  deriving Show

data CFGNode = CFGNode Int Text NodeType [CFGNode] [CFGNode]
    deriving Show