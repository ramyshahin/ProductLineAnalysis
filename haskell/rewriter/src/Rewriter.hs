module Rewriter where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
    
import Control.Reference ((^.), (!~), (.-), (&))
import PrelNames (dollarName)
import SrcLoc (RealSrcSpan)

run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . rewrite)
    
rewrite :: RealSrcSpan -> LocalRefactoring
rewrite sp = return . (nodesContained sp .- lift) -- & annList .- liftDeclaration)

--liftDeclaration :: Decl -> Decl
--liftDeclaration d = d

--liftBind :: ValueBind -> ValueBind
--liftBind (SimpleBind pat rhs locals) = mkSimpleBind pat rhs (locals ^. annMaybe)

--liftOp :: Operator -> Expr
--liftOp op = mkApp (mkUnqualOp' (mkName "mkVarT")) op

appOp =  mkUnqualOp "<*>"

mkVarT = mkVar (mkName "mkVarT")
apply  = mkVar (mkName "apply")
apply2 = mkVar (mkName "apply2")

liftOp (NormalOp o) = mkApp apply2 (mkParen (mkApp mkVarT (mkVar (mkParenName o))))

lift :: Expr -> Expr
--liftExpr (ModuleHead name pragmas exports) = mkModuleHead (name ++ "\'") pragmas exports

lift (App fun arg) = mkInfixApp fun appOp arg

lift (InfixApp arg1 op arg2) = 
    mkApp (mkApp (liftOp op) arg1) arg2

lift (Lit l) = mkParen $ mkApp mkVarT (mkLit l)

lift e = e 