module Rewriter where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
    
import Control.Reference ((^.), (!~), (.-), (&))
import PrelNames (dollarName)
import SrcLoc (RealSrcSpan)

run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . rewrite)
    
rewrite :: RealSrcSpan -> LocalRefactoring
rewrite sp = return . (nodesContained sp .- liftExpr) -- & annList .- liftDeclaration)

--liftDeclaration :: Decl -> Decl
--liftDeclaration d = d

--liftBind :: ValueBind -> ValueBind
--liftBind (SimpleBind pat rhs locals) = mkSimpleBind pat rhs (locals ^. annMaybe)

--liftOp :: Operator -> Expr
--liftOp op = mkApp (mkUnqualOp' (mkName "mkVarT")) op

appOp = (mkUnqualOp "<*>")

liftExpr :: Expr -> Expr
--liftExpr (ModuleHead name pragmas exports) = mkModuleHead (name ++ "\'") pragmas exports

liftExpr (App fun arg) = mkInfixApp fun appOp arg
--liftExpr (InfixApp arg1 op arg2) = 
--    return $ mkInfixApp (mkInfixApp (liftOp op) (mkUnqualOp "<*>") arg1) 
--                        (mkUnqualOp "<*>") arg2

liftExpr e = e 