module Rewrite.Common where 

import Language.Haskell.Tools.Refactor
import Control.Reference ((^.))

moduleNameSPL = mkModuleName "SPL"
importSPL = mkImportDecl False False False Nothing moduleNameSPL Nothing Nothing

appendModName :: String -> ModuleName -> ModuleName
appendModName s mn = mkModuleName $ (mn ^. moduleNameString) ++ s

appOp  = mkUnqualOp "<*>"
mkVarT = mkVar (mkName "mkVarT")
tyVar  = mkVarType $ mkName "Var"

pat2expr (VarPat n) = mkVar n 
