module Rewrite.Common where 

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST.Ann
import Control.Reference ((^.), (.-))
import qualified SPL as L 
import qualified Data.Set as S 

moduleNameSPL = mkModuleName "SPL"
importSPL :: ImportDecl
importSPL = mkImportDecl False False False Nothing moduleNameSPL Nothing Nothing

prependModName :: String -> ModuleName -> ModuleName
prependModName s mn = mkModuleName $ s++ (mn ^. moduleNameString)

appOp  = mkUnqualOp "<*>"
fmapOp = mkUnqualOp "<$>"
mkVarT = mkVar (mkName "mkVarT")
mkVars = mkVar (mkName "mkVars")
--tt     = mkVar (mkName "tt")
tyVar  = mkVarType $ mkName "V"

pat2expr (VarPat n) = mkVar n 

-- | Rename module
--
updateHead :: String -> Maybe ModuleHead -> Maybe ModuleHead
updateHead prefix mh =  
    case mh of
        Just mh' -> Just $ (mhName .- (prependModName prefix)) $ mh'
        _ -> mh
    
renameModule :: String -> Module -> Module
renameModule suffix mod = (modHead .- (annMaybe .- (updateHead suffix))) mod

