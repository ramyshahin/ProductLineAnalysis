
{-# LANGUAGE LambdaCase #-}

module Rewrite.Deep where

import Rewrite.Common
    
-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation
    
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
    
import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace 

deepRewrite :: RefactoringChoice
deepRewrite = ModuleRefactoring "DeepRewrite" (localRefactoring deep)
    
run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . (\_ -> deep))
    
deep :: LocalRefactoring
deep mod = do
    let decls = getLocalDeclarations mod
    return  $ filePragmas & annList     .- rewritePragma 
            $ modHead     & annMaybe    .- rewriteHeader
            $               modImports  .- rewriteImports
            $ modDecl     & annList     .- (rewriteDecl decls)
            $ mod 
    
getLHSName :: Decl -> Name
getLHSName d =
    case d of
        ValueBinding (SimpleBind (VarPat p) _ _) -> 
            trace ("Decl: " ++ prettyPrint p) $ p
        _ -> mkName ""

getLocalDeclarations :: Module -> [Name]
getLocalDeclarations mod = do
    let decls = _annListElems $ mod ^. modDecl
    map getLHSName decls
    
-- | Rewriting imports
--
imports xs = map snd (zipWithSeparators xs)
    
rewritePragma = id

rewriteHeader :: Maybe ModuleHead -> Maybe ModuleHead 
rewriteHeader header =
    case header of
        Nothing -> Nothing
        Just h  -> Just $ mhName .- (appendModName "Deep") $ h

rewriteImports :: ImportDeclList  -> ImportDeclList 
rewriteImports xs = (annListElems .= concat [[importSPL], (imports xs)]) xs 
    
-- | Rewrite declarations
--
rewriteDecl :: [Name] -> Decl -> Decl
rewriteDecl decls d = 
     case d of
        TypeSigDecl sig -> rewriteTypeSig sig
        ValueBinding vb -> rewriteValueBind decls vb
        _ -> trace ("Unhandled Decl " ++ prettyPrint d) $ d
        -- TODO: other cases

rewriteValueBind :: [Name] -> ValueBind -> Decl
rewriteValueBind decls vb = mkValueBinding $ case vb of
    SimpleBind p rhs bs -> mkSimpleBind p (rewriteRhs decls rhs) (_annMaybe bs)
    FunctionBind ms -> mkFunctionBind (map (rewriteMatch decls) (_annListElems ms)) 
    _ -> trace ("Unhandled Value Bind " ++ prettyPrint vb) $ vb

rewriteRhs :: [Name] -> Rhs -> Rhs
rewriteRhs decls rhs = case rhs of
    UnguardedRhs e -> mkUnguardedRhs $ rewriteExpr decls e
    _              -> trace ("Unhandled RHS " ++ prettyPrint rhs) $ rhs

rewriteMatch :: [Name] -> Match -> Match
rewriteMatch decls (Match lhs rhs binds) = 
    mkMatch lhs (rewriteRhs decls rhs) (_annMaybe binds)

liftedCond = mkVar (mkName "liftedCond")
liftedNeg = mkVar (mkName "liftedNeg")
liftedCase = mkVar (mkName "liftedCase")

liftOp (NormalOp o) = mkParen (mkApp mkVarT (mkVar (mkParenName o)))

rewriteVar :: [Name] -> Name -> Expr
rewriteVar declarations vn = 
    if   (externalVar declarations vn)
    then mkApp mkVarT (mkVar vn)
    else mkVar vn

externalFun :: [Name] -> Expr -> Bool
externalFun xs x = 
    case x of
        Var n -> trace ("Internal " ++ prettyPrint x) $ not $ any (== n) xs
        _     -> trace ("External " ++ prettyPrint x) $ True

externalVar :: [Name] -> Name -> Bool
externalVar xs x = 
    let r = not $ any (== x) xs
    in  trace ("Checking " ++ prettyPrint x ++ " : " ++ show r) $ r

-- lifting expressions 
rewriteExpr :: [Name] -> Expr -> Expr
rewriteExpr declarations e = 
    case e of 
        Lit l -> mkParen $ mkApp mkVarT (mkLit l)
        Var n -> if (externalVar declarations n) 
                 then rewriteVar declarations n 
                 else e
        InfixApp arg1 op arg2 -> 
            mkInfixApp (mkInfixApp 
                            (liftOp op) 
                            appOp 
                            (rewriteExpr declarations arg1)) 
                        appOp 
                        (rewriteExpr declarations arg2)
        PrefixApp op arg -> mkInfixApp liftedNeg appOp arg
        App fun arg -> if (externalFun declarations fun)
                       then mkInfixApp (rewriteExpr declarations fun) appOp arg
                       else mkApp fun (rewriteExpr declarations arg)
        If c t e -> mkParen $ 
                        mkApp (mkApp 
                                (mkApp 
                                    liftedCond (rewriteExpr declarations c))
                                               (mkParen (rewriteExpr declarations t)))
                                               (mkParen (rewriteExpr declarations e))
        Case v alts -> 
            --let as   = _annListElems alts
            --    ls   = map (\(Alt p (CaseRhs rhs) _) -> mkLambda [p] rhs) as
            --    ls'  = mkVars $ map (\l -> (l,tt)) ls 
            --in  mkParen $ mkInfixApp ls' appOp v 
                    mkApp (mkApp liftedCase (mkParen (mkLambdaCase (_annListElems alts)))) v
        MultiIf alts -> trace "Unhandled MultiIf" e 
        Lambda b e -> trace "Unhandled Lambda" e
        Let b e -> trace "Unhandled Let" e
        Do ss -> trace "Unhandled Do" e
        Tuple es -> trace "Unhandled Tuple" e 
        UnboxedTuple es -> trace "Unhandled UnboxedTuple" e 
        TupleSection es -> trace "Unhandled TupleSelection" e 
        UnboxedTupleSection es -> trace "Unhandled UnboxedTupSec" e 
        --List es -> mkApp mkVarT (mkList $ map (rewriteExpr declarations) (_annListElems es))
        List es -> mkApp mkVarT e
        ParArray es -> trace "Unhandled ParArray" e
        Paren ex -> mkParen (rewriteExpr declarations ex)
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
        _ -> trace ("Unhandled Expr " ++ prettyPrint e) $ e
