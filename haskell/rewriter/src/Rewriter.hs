module Rewriter where

-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
    
import Control.Reference ((.-), (.=))
import SrcLoc (RealSrcSpan)

run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . rewrite)
    
rewrite :: RealSrcSpan -> LocalRefactoring
rewrite sp = return . (nodesContained sp .- liftDecl) . 
                      (nodesContained sp .- lift) .
                      (nodesContained sp .- updateImports) .
                      (nodesContained sp .- updateModName)

updateModName :: ModuleHead -> ModuleHead
updateModName (ModuleHead (ModuleName n) _ _) = 
    mkModuleHead (mkModuleName (n ++ "Deep")) Nothing Nothing

moduleNameSPL = mkModuleName "SPL"

importSPL = mkImportDecl False False False Nothing moduleNameSPL Nothing Nothing

imports xs = map snd (zipWithSeparators xs)

updateImports :: ImportDeclList -> ImportDeclList
updateImports xs = (annListElems .= concat [[importSPL], (imports xs)]) xs

tyVar = mkVarType $ mkName "Var"

liftType t = mkTypeApp tyVar t

--liftBind :: ValueBind -> ValueBind
--liftBind (SimpleBind pat rhs locals) = mkSimpleBind pat rhs (locals ^. annMaybe)

--liftOp :: Operator -> Expr
--liftOp op = mkApp (mkUnqualOp' (mkName "mkVarT")) op

appOp =  mkUnqualOp "<*>"

mkVarT = mkVar (mkName "mkVarT")
apply  = mkVar (mkName "liftA")
apply2 = mkVar (mkName "liftA2")

liftOp (NormalOp o) = mkParen (mkApp mkVarT (mkVar (mkParenName o)))
liftExpr e = mkParen (mkApp mkVarT e)

--liftExpr (ModuleHead name pragmas exports) = mkModuleHead (name ++ "\'") pragmas exports

liftDecl t@(TypeApp t1 t2) = mkTypeApp (liftType t1) t2 
liftDecl t@(VarType _) = liftType t
liftDecl d = d

externalFun fun = False

lift a@(App fun arg) = if (externalFun fun)
                       then mkInfixApp (liftExpr fun) appOp arg
                       else a

lift (InfixApp arg1 op arg2) = 
    mkInfixApp (mkInfixApp (liftOp op) appOp arg1) appOp arg2

lift (PrefixApp op arg) =
        mkInfixApp (liftExpr (mkParen (mkLambda [(mkVarPat (mkName "x"))] (mkPrefixApp op (mkVar (mkName "x")))))) appOp arg

lift (Lit l) = mkParen $ mkApp mkVarT (mkLit l)

lift e = e 
