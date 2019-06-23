
{-# LANGUAGE LambdaCase #-}

module Rewrite.Deep where

import Rewrite.Common
    
-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation
    
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
    
import Control.Reference ((.-), (.=), (^.))
import FastString
import Debug.Trace 

deepRewrite :: RefactoringChoice
deepRewrite = ModuleRefactoring "DeepRewrite" (localRefactoring deep)
    
run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . (\_ -> deep))
    
deep :: LocalRefactoring
deep mod = do
    let pragmas = mod ^. filePragmas
    let head = _annMaybe $ mod ^. modHead
    let name = case head of
                    Nothing -> mkModuleName ""
                    Just h -> h ^. mhName
    let mod' = modImports .- (rewriteImports name) $ mod
    let renamedMod = renameModule "Deep" mod'
    return $ (modDecl .- (annList .- rewriteDecl)) renamedMod
    
-- | Rewriting imports
--
imports xs = map snd (zipWithSeparators xs)
    
rewriteImports :: ModuleName -> ImportDeclList  -> ImportDeclList 
rewriteImports n xs = (annListElems .= concat [[importSPL], (imports xs)]) xs 
    
-- | Rewrite declarations
--
rewriteMatch :: Match -> Match
rewriteMatch (Match lhs rhs binds) = 
    mkMatch lhs (rewriteRhs rhs) (_annMaybe binds)

rewriteRhs :: Rhs -> Rhs
rewriteRhs (UnguardedRhs e) = mkUnguardedRhs $ rewriteExpr e

rewriteValueBind :: ValueBind -> Decl
rewriteValueBind vb = mkValueBinding $ case vb of
    SimpleBind p rhs bs -> mkSimpleBind p (rewriteRhs rhs) (_annMaybe bs)
    FunctionBind ms -> mkFunctionBind (map rewriteMatch (_annListElems ms)) 

rewriteDecl :: Decl -> Decl
rewriteDecl d = 
     case d of
        TypeSigDecl sig -> rewriteTypeSig sig
        ValueBinding vb -> rewriteValueBind vb
        _ -> d
        -- TODO: other cases

liftedCond = mkVar (mkName "liftedCond")
liftedNeg = mkVar (mkName "liftedNeg")
liftedCase = mkVar (mkName "liftedCase")

liftOp (NormalOp o) = mkParen (mkApp mkVarT (mkVar (mkParenName o)))

rewriteVar :: Name -> Expr
rewriteVar vn = mkApp mkVarT (mkVar vn)

externalFun fun = False

externalVar v = False 

-- lifting expressions 
rewriteExpr :: Expr -> Expr
rewriteExpr e = 
    case e of 
        Var n -> if externalVar n then rewriteVar n else e
        Lit l -> mkParen $ mkApp mkVarT (mkLit l)
        InfixApp arg1 op arg2 -> mkInfixApp (mkInfixApp (liftOp op) appOp arg1) appOp arg2
        PrefixApp op arg -> mkInfixApp liftedNeg appOp arg
        App fun arg -> if (externalFun fun)
                       then mkInfixApp (rewriteExpr fun) appOp arg
                       else mkApp fun (rewriteExpr arg)
        Lambda b e -> trace "Unhandled Lambda" e
        Let b e -> trace "Unhandled Let" e
        If c t e -> mkParen $ 
                        mkApp (mkApp 
                                (mkApp 
                                    liftedCond (rewriteExpr c))
                                               (mkParen (rewriteExpr t)))
                                               (mkParen (rewriteExpr e))
        MultiIf alts -> trace "Unhandled MultiIf" e 
        Case v alts -> mkParen $
                        mkApp (mkApp liftedCase (mkParen (mkLambdaCase (_annListElems alts)))) v
        Do ss -> trace "Unhandled Do" e
        Tuple es -> trace "Unhandled Tuple" e 
        UnboxedTuple es -> trace "Unhandled UnboxedTuple" e 
        TupleSection es -> trace "Unhandled TupleSelection" e 
        UnboxedTupleSection es -> trace "Unhandled UnboxedTupSec" e 
        List es -> trace "Unhandled List" e
        ParArray es -> trace "Unhandled ParArray" e
        Paren ex -> mkParen (rewriteExpr ex)
        LeftSection lhs o -> trace "Unhandled LeftSection" e
        RightSection o rhs -> trace "Unhandled RightSection" e
        RecCon r fs -> trace "Unhandled RecCon" e
        Enum fr th to -> trace "Unhandled Enum" e 
        ParArrayEnum fr th to -> trace "Unhandled ParArrayEnum" e
        ListComp ex b -> trace "Unhandled ListComp" e 
        TypeSig ex s -> trace "Unhandled TypeSig" e 
        ExplicitTypeApp ex t -> trace "Unhandled ExplTypeApp" e 
        VarQuote n -> trace "Unhandled VarQuote" e 
        TypeQuote t -> trace "Unhandled TypeQuote" e 
        BracketExpr ex -> trace "Unhandled BracketExpr" e 
        SpliceExpr s -> trace "Unhandled Splice" e 
        QuasiQuoteExpr q -> trace "Unhandled QuasiQuoteExpr" e 
        ExprPragma p e -> trace "Unhandled ExprPragma" e 
        Proc p ex -> trace "Unhandled Proc" e 
        ArrowApp l a r -> trace "Unhandled ArrowApp" e 
        LambdaCase alts -> trace "Unhandled LamCase" e 
        StaticPointer e -> trace "Unhandled StaticPtr" e 
        --UnboxedSum s i ps -> trace "Unhandled UnboxedSum" e 
        Hole -> trace "Unhandled Hole" e
