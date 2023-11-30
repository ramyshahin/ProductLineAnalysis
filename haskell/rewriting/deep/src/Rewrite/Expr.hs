module Rewrite.Expr where

import Data.Char (isUpper)
import Rewrite.Base
import Rewrite.Bind
import Rewrite.Pattern 
import Rewrite.ValueBind

import qualified Data.Set as S

restrictExpr :: Expr -> Expr
restrictExpr e = mkParen $ mkInfixApp e restrictOp cntxtExpr

liftExpr :: Declarations -> Declarations -> Bool -> Expr -> Expr
liftExpr globals locals inBranch e = mkParen $ 
    --let pc = if inBranch then cntxtExpr else cntxtExpr --ttExpr
    --in  mkInfixApp e upOp pc  
    mkV e 

isDeepExpr :: Expr -> Bool
isDeepExpr e = 
    case e of
        Var n -> let vn = prettyPrint n
                     h  = head vn
                 in (h == '_')
        _     -> False

mkAltBinding :: Declarations -> Declarations -> Integer -> Alt -> LocalBind
mkAltBinding globals locals index (Alt p (CaseRhs rhs) _) = 
    let lhsName = mkName $ "case" ++ (show index)
        splitName = mkName $ "split" ++ (show index)
        params' = getPatternVars p
        params = S.toList $ params'
        lhs = mkMatchLhs lhsName (map (mkVarPat . mkName) (vCntxt : params))
        dummy = mkName dummyVar
        splitLhs = mkMatchLhs splitName [mkVarPat dummy]
        splitAlt = mkAlt p (mkCaseRhs (mkTuple (map (mkVar . mkName) params))) Nothing
    in  mkLocalValBind $ mkFunctionBind [mkMatch lhs (mkUnguardedRhs (rewriteBranch globals (S.union locals params') rhs)) Nothing,
                                         mkMatch splitLhs (mkUnguardedRhs (mkCase (mkVar dummy) [splitAlt])) Nothing]

rewritePrimitiveFuncName :: String -> Expr
rewritePrimitiveFuncName s = mkVar $ mkName (s) -- ++ "\'")

rewriteVar :: Declarations -> Declarations -> Bool -> Name -> Expr
rewriteVar globals locals inBranch vn = 
    let v = prettyPrint vn
        vFst = head v
        e = mkVar vn
        r = (not $ S.member v globals) &&
            (vFst /= '_') && (not $ isUpper vFst) -- HACK 
    in  if      isPrimitiveFunc v
        then    rewritePrimitiveFuncName v
        else    if      externalDecl globals locals vn
                then    liftExpr globals locals (inBranch) e
                else    if inBranch && r then restrictExpr e else e
                
rewriteBranch :: Declarations -> Declarations -> Expr -> Expr
rewriteBranch globals locals e = 
     rewriteExpr globals locals True e

{-
rewriteInfixApp :: Declarations -> Declarations -> Bool -> Expr -> Operator -> Expr -> Expr
rewriteInfixApp globals locals inBranch lhs op rhs =
    let lhs' = rewriteExpr globals locals inBranch lhs
        rhs' = rewriteExpr globals locals inBranch rhs
        rewriteIt = mkInfixApp    (mkInfixApp (liftOp op inBranch) appOp (mkParen lhs')) 
                                        appOp 
                                        (mkParen $ rhs')
    in  case op of
            NormalOp n  -> 
                if      isPrimitiveOp (prettyPrint n)
                then    mkInfixApp lhs' (getLiftedPrimitiveOp (prettyPrint n)) rhs'
                else    rewriteIt
            _           -> rewriteIt
-}

rewriteAlt' :: Declarations -> Declarations -> Bool -> Alt -> Alt
rewriteAlt' globals locals inBranch (Alt p (CaseRhs e) xs) =
    let params' = getPatternVars p
        --params = S.toList $ params'
        rhs     = mkCaseRhs $ rewriteExpr globals (S.union locals params') inBranch e
    in  mkAlt p rhs (_annMaybe xs)

rewriteAlt :: Declarations -> Declarations -> Expr -> Alt -> Expr
rewriteAlt globals locals c (Alt p (CaseRhs e) _) =
    let e' = rewriteExpr globals locals False e
        p' = rewriteCasePattern p
        accessor = attributeName $ getPatternName p 
        field = mkParen $ mkApp (mkVar (accessor)) (mkParen c) 
    in  mkApp (mkParen (mkLambda [p'] e'))field 

rewriteCase :: Declarations -> Declarations -> Expr -> [Alt] -> Expr
rewriteCase globals locals c alts =
    mkApp symUnions $
    mkList $ map (rewriteAlt globals locals c) alts

rewriteLocalBind :: Declarations -> Declarations -> Bool -> LocalBind -> (LocalBind, S.Set String)
rewriteLocalBind globals locals inBranch lb =
    let vbs = S.union locals $ getLocalVars lb
    in  case lb of 
            LocalValBind (SimpleBind p (UnguardedRhs rhs) xs) -> 
                (mkLocalValBind (mkSimpleBind p (mkUnguardedRhs (rewriteExpr globals locals inBranch rhs)) (_annMaybe xs)), vbs)
            _ -> trace ("Unsupported Local Bind: " ++ prettyPrint lb) $ (lb, S.empty)

rewriteMatch :: Declarations -> Declarations -> Bool -> Match -> Match
rewriteMatch globals locals inBranch (Match lhs rhs binds) = 
    mkMatch lhs (rewriteRhs globals locals inBranch rhs) (_annMaybe binds)

rewriteRhs :: Declarations -> Declarations -> Bool -> Rhs -> Rhs
rewriteRhs globals locals inBranch rhs = case rhs of
    UnguardedRhs e -> mkUnguardedRhs $ rewriteExpr globals locals inBranch e
    _              -> trace ("Unhandled RHS " ++ prettyPrint rhs) $ rhs

rewriteExpr :: Declarations -> Declarations -> Bool -> Expr -> Expr
rewriteExpr globals locals inBranch e = 
    case e of 
        Lit l -> liftExpr globals locals inBranch e
        Var n -> mkVar n -- rewriteVar globals locals inBranch n 
        -- assuming all infix operators have been lifted, either in 
        -- VPrelude or in the module being lifted
        InfixApp arg1 op arg2 -> mkInfixApp arg1 op arg2 --rewriteInfixApp globals locals inBranch arg1 op arg2
        PrefixApp op arg -> mkPrefixApp op arg -- mkInfixApp liftedNeg appOp arg
        App fun arg ->  let fun' = rewriteExpr globals locals inBranch fun
                            arg' = rewriteExpr globals locals inBranch arg
                        in  mkApp fun' arg'
                            {-
                            case fun of
                             
                                Var n -> if (externalDecl globals locals n)
                                         then   if isPrimitiveFunc (prettyPrint n)
                                                then mkApp (rewritePrimitiveFuncName (prettyPrint n)) arg'
                                                else mkInfixApp fun' appOp arg'
                                         else mkApp fun' arg'
                                _ -> case fun' of 
                                            App _ _ -> mkApp fun' arg'
                                            InfixApp _ op _ -> mkInfixApp fun' appOp arg' 
                                            _       -> mkInfixApp fun' appOp arg'
                                            -}
        If c t e -> mkApp   (mkApp  
                        (mkApp  liftedCond  (mkParen (rewriteExpr globals locals inBranch c)))
                        (mkParen $ mkLambda [cntxtPat] (rewriteBranch globals locals t)))
                        (mkParen $ mkLambda [cntxtPat] (rewriteBranch globals locals e))
        Case v alts -> rewriteCase globals locals v (_annListElems alts) 
            {-
            if isDeepExpr v then mkCase v $ map (rewriteAlt' globals locals inBranch) $ _annListElems alts
            else
            let dummy    = mkName dummyVar
                arg      = mkVarPat $ dummy
                splitter = mkParen $ mkLambda [arg] (mkCase (mkVar dummy) (splitAlts 0 $ _annListElems alts))
                as       = map (rewriteAlt globals locals) $ (zip [0..] $ _annListElems alts)
                v'       = mkParen (rewriteExpr globals locals inBranch v)
            in  mkLet (map (\(a,i) -> mkAltBinding globals locals a i) (zip [0..] (_annListElems alts)))
                      (mkApp (mkApp (mkApp liftedCase v') splitter) (mkList as))
            -}
        MultiIf alts -> trace "Unhandled MultiIf" e 
        Lambda b e -> let vbs = foldr S.union S.empty (map getPatternVars (_annListElems b))
                          locals' = S.union locals vbs 
                      in mkLambda (_annListElems b) (rewriteExpr globals locals' inBranch e)
        Let bs e -> 
            let bs' = foldl (\xs b -> 
                                let   ls =  if null xs 
                                            then locals 
                                            else S.union locals (snd (head xs))
                                in    (rewriteLocalBind globals ls inBranch b) : xs) 
                            [] $ _annListElems bs
                ls  = snd (head bs')
                e'  = rewriteExpr globals ls inBranch e
            in  mkLet ((reverse . fst . unzip) bs') e'
        Do ss -> trace "Unhandled Do" e
        Tuple es -> trace "Unhandled Tuple" e 
        UnboxedTuple es -> trace "Unhandled UnboxedTuple" e 
        TupleSection es -> trace "Unhandled TupleSelection" e 
        UnboxedTupleSection es -> trace "Unhandled UnboxedTupSec" e 
        --List es -> mkApp mkVarT (mkList $ map (rewriteExpr globals locals) (_annListElems es))
        List es -> mkList (map (rewriteExpr globals locals inBranch) (_annListElems es)) -- liftExpr globals locals inBranch e
        ParArray es -> trace "Unhandled ParArray" e
        Paren ex -> mkParen (rewriteExpr globals locals inBranch ex)
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
