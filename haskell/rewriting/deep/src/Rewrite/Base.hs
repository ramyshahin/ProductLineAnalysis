module Rewrite.Base where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
import Language.Haskell.Tools.Rewrite.Match.Decls

import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace 
import qualified Data.Set as S 
import qualified SPL as L

--notSupported :: a -> a
notSupported x = trace ("Not supported: " ++ prettyPrint x) x

type Declarations = S.Set String

innerName :: Name -> Name
innerName n = mkName (prettyPrint n ++ "_")

consName :: Name -> Name
consName n = mkName ("_" ++ prettyPrint n)

mkVarTOp = mkVar (mkName "mkVarT")
compOp   = mkUnqualOp "."
