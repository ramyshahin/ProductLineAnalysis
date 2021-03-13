{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module NodeTypes where

import Language.C.Syntax.AST
import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Text as T

data NodeType =
    CFGExpr     CExpr
  | CFGStat     CStat
  | CFGVarDecl  CExtDecl
  | CFGDecl     T.Text
  | CFGFunc     T.Text
  | CFGFuncRoot T.Text
  | CFGDummy    T.Text
    deriving (Show, Generic, NFData)