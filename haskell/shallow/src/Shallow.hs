module Shallow where

-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST.Ann 

import Control.Reference ((.-), (.=))
import SrcLoc (RealSrcSpan)

run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . rewrite)
    
rewrite :: RealSrcSpan -> LocalRefactoring
rewrite sp = return . (nodesContained sp .- liftDecl) . 
                      (nodesContained sp .- liftExpr) .
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

-- lifting types
liftType t = mkParenType $ mkTypeApp tyVar t

appOp =  mkUnqualOp "<*>"

mkVarT = mkVar (mkName "mkVarT")
apply  = mkVar (mkName "liftA")
apply2 = mkVar (mkName "liftA2")
liftedCond = mkVar (mkName "liftedCond")
liftedNeg = mkVar (mkName "liftedNeg")
liftedCase = mkVar (mkName "liftedCase")

liftOp (NormalOp o) = mkParen (mkApp mkVarT (mkVar (mkParenName o)))

--liftExpr :: Expr -> Expr
--liftExpr e@(Lit _) = mkParen (mkApp mkVarT e)
--liftExpr (If c t e) = mkParen (mkApp (mkApp (mkApp liftedCond c) t) e)
--liftExpr e = e 

--liftExpr (ModuleHead name pragmas exports) = mkModuleHead (name ++ "\'") pragmas exports

--liftDecl :: Decl -> Decl 
liftDecl t@(TypeApp t1 t2) = mkTypeApp (liftType t1) t2 
liftDecl t@(VarType _) = liftType t
liftDecl d = d

externalFun fun = False

-- lifting expressions 
liftExpr a@(App fun arg) = if (externalFun fun)
                       then mkInfixApp (liftExpr fun) appOp arg
                       else a

liftExpr (InfixApp arg1 op arg2) = 
    mkInfixApp (mkInfixApp (liftOp op) appOp arg1) appOp arg2

liftExpr (PrefixApp op arg) =
        mkInfixApp liftedNeg appOp arg

liftExpr c@(Case v alts) = mkParen $
    mkApp (mkApp liftedCase (mkParen (mkLambdaCase (_annListElems alts)))) v

liftExpr (Lit l) = mkParen $ mkApp mkVarT (mkLit l)

liftExpr (If c t e) = mkParen (mkApp (mkApp (mkApp liftedCond c) (mkParen t)) (mkParen e))

liftExpr e = e 
