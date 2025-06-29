module Rewrite.Expr where

import Data.Char (isUpper)
import Rewrite.Base
import Rewrite.Bind
import Rewrite.Pattern 
import Rewrite.ValueBind

import qualified Data.Set as S

isVOp :: Expr -> Bool
isVOp e = defaultLift == e 

liftExpr :: Declarations -> Declarations -> Bool -> Bool -> Expr -> Expr
liftExpr globals locals inConstructor bRestrict e = 
    if bRestrict
    then mkParen $ mkInfixApp e upOp cntxtExpr
    else mkV e 

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
    in  mkLocalValBind $ mkFunctionBind [mkMatch lhs (mkUnguardedRhs (rewriteBranch globals (S.union locals params') True rhs)) Nothing,
                                         mkMatch splitLhs (mkUnguardedRhs (mkCase (mkVar dummy) [splitAlt])) Nothing]

rewritePrimitiveFuncName :: String -> Expr
rewritePrimitiveFuncName s = mkVar $ mkName (s) -- ++ "\'")

mkRestrictExpr :: Expr -> Expr
mkRestrictExpr e = mkParen $ mkInfixApp e restrictOp cntxtExpr

mkLiftedExpr :: Expr -> Expr
mkLiftedExpr e = mkParen $ mkInfixApp e upOp cntxtExpr

{-
rewriteVar :: Declarations -> Declarations -> Bool -> Bool -> Name -> Expr
rewriteVar globals locals inConstructor bRestrict vn = 
    let v = prettyPrint vn
        vFst = head v
        e = mkVar vn
        r = (not $ S.member v globals) &&
            (vFst /= '_') && (not $ isUpper vFst) -- HACK 
    in  if      isPrimitiveFunc v
        then    rewritePrimitiveFuncName v
        else    if      externalDecl globals locals vn
                then    liftExpr globals locals (inConstructor) e
                else    if bRestrict then mkRestrictExpr e else e
  -}

rewriteBranch :: Declarations -> Declarations -> Bool -> Expr -> Expr
rewriteBranch globals locals bRestrict e = 
     rewriteExpr globals locals False bRestrict e

{-
rewriteInfixApp :: Declarations -> Declarations -> Bool -> Expr -> Operator -> Expr -> Expr
rewriteInfixApp globals locals inConstructor lhs op rhs =
    let lhs' = rewriteExpr globals locals inConstructor lhs
        rhs' = rewriteExpr globals locals inConstructor rhs
        rewriteIt = mkInfixApp    (mkInfixApp (liftOp op inConstructor) appOp (mkParen lhs')) 
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
rewriteAlt' globals locals inConstructor (Alt p (CaseRhs e) xs) =
    let params' = getPatternVars p
        --params = S.toList $ params'
        rhs     = mkCaseRhs $ rewriteExpr globals (S.union locals params') inConstructor True e
    in  mkAlt p rhs (_annMaybe xs)

rewriteAlt :: Declarations -> Declarations -> Expr -> Alt -> Alt
rewriteAlt globals locals c (Alt p (CaseRhs e) _) =
    mkAlt (rewriteCasePattern p) (mkCaseRhs (rewriteExpr globals locals False True e)) Nothing
{-
    let e' = rewriteExpr globals locals False e
        p' = mkParenPat $ 
                mkAppPat presentCons [mkTuplePat [rewriteCasePattern p, (mkVarPat . mkName) "pc"] ]
        accessor = attributeName $ getPatternName p 
        field = mkParen $ mkApp (mkVar (accessor)) (mkParen c) 
        r = mkVarPat $ mkName "r"
    in  --mkInfixApp (mkParen (mkLambda [p'] e')) fmapOp field
        mkApp (mkParen (mkLambda [p', r] e')) field
-}

expr2name :: Expr -> Name
expr2name e =
    case e of 
        Var v -> v
        _ -> trace ("Unsupported expr2name expression: " ++ prettyPrint e) $ mkName ""

expr2pat :: Expr -> Pattern 
expr2pat e =
    case e of
        Var v -> mkVarPat v
        App f a -> mkAppPat (expr2name f) [(expr2pat a)]
        _ -> trace ("Unsupported expr2pat expression: " ++ prettyPrint e) $ mkVarPat (mkName "")

rewriteCase :: Declarations -> Declarations -> Expr -> [Alt] -> Expr
rewriteCase globals locals c alts =
    let --w = mkName "v"
        pc = mkName "pc"
    in mkApp (mkApp symMatch c)
          (mkParen (mkLambda [mkTuplePat [expr2pat c, mkVarPat pc]]
                    (mkLet [mkLocalValBind (mkSimpleBind cntxtPat (mkUnguardedRhs (mkInfixApp cntxtExpr conj (mkVar pc))) Nothing)]
                        
                        (mkCase (rewriteExpr globals locals False False c) (map (rewriteAlt globals locals c) alts))
          )))

rewriteLocalBind :: Declarations -> Declarations -> Bool -> Bool -> LocalBind -> (LocalBind, S.Set String)
rewriteLocalBind globals locals inConstructor bRestrict lb =
    let vbs = S.union locals $ getLocalVars lb
    in  case lb of 
            LocalValBind (SimpleBind p (UnguardedRhs rhs) xs) -> 
                (mkLocalValBind (mkSimpleBind p (mkUnguardedRhs (rewriteExpr globals locals inConstructor bRestrict rhs)) (_annMaybe xs)), vbs)
            _ -> trace ("Unsupported Local Bind: " ++ prettyPrint lb) $ (lb, S.empty)

rewriteMatch :: Declarations -> Declarations -> Bool -> Match -> Match
rewriteMatch globals locals inConstructor (Match lhs rhs binds) = 
    mkMatch lhs (rewriteRhs globals locals inConstructor False rhs) (_annMaybe binds)

rewriteRhs :: Declarations -> Declarations -> Bool -> Bool -> Rhs -> Rhs
rewriteRhs globals locals inConstructor bRestrict rhs = case rhs of
    UnguardedRhs e -> mkUnguardedRhs $ rewriteExpr globals locals inConstructor bRestrict e
    _              -> trace ("Unhandled RHS " ++ prettyPrint rhs) $ rhs

isConstructorName :: Name -> Bool
isConstructorName n = isUpper ((head . prettyPrint) n)

isConstructor :: Expr -> Bool
isConstructor e = 
    case e of 
        Var n   -> isConstructorName n
        App f x -> isConstructor f 
        _       -> False

rewriteVar' :: Name -> Expr
rewriteVar' n = 
    let i_name = innerName n 
        --x = mkParen $ mkApp (mkVar presentCons) (mkTuple [mkVar i_name, (mkVar . mkName) "pc"])
        --r = mkVar $ mkName "r"
    in  mkVar $ 
        if isConstructorName n 
        then i_name -- mkApp (mkApp (mkVar $ consFnName i_name) x) r
        else n

{-
getConsName :: Expr -> Name
getConsName e =
    case e of
        Var n -> n
        App f _ -> getConsName f --(mkApp (rewriteExpr globals locals inConstructor f) (rewriteExpr globals locals inConstructor a), innerName n')
        _ -> trace ("Unhandled Expr " ++ prettyPrint e) $ mkName ""
-}

{-
rewriteConstructor :: Expr -> Expr
rewriteConstructor e = liftExpr globals locals inConstructor e

    let i_name = getConsName e
                --let i_name = if isConstructorName n then innerName n else n
        pc = "allConfigs" -- "pc"
        x = mkParen $ mkApp (mkVar presentCons) (mkTuple [e, (mkVar . mkName) pc])
        r = mkVar $ mkName "nil" --"r"
    in  mkParen $ mkApp (mkApp (mkVar $ consFnName i_name) x) r
        --mkApp x r
-}

--restrictExpr :: Expr -> Expr
--restrictExpr e = 
    --case e of
        --Var n -> mkParen $ mkInfixApp e (if isConstructorName n then upOp else restrictOp) cntxtExpr
        --InfixApp arg1 op arg2 -> mkApp toSubV (mkParen (mkInfixApp (restrictExpr arg1) op (restrictExpr arg2)))
        --PrefixApp op arg -> mkApp liftedNeg (restrictExpr arg)
        --App fun arg ->  
        --    if isVOp fun 
        --    then restrictExpr arg    
        --    else mkApp fun (restrictExpr arg)
        --Paren e -> mkParen (restrictExpr e) 
        --Lambda _ _ -> e
        --_ -> trace ("restrictExpr: Unhandled Expr " ++ prettyPrint e) $ e

rewriteExpr :: Declarations -> Declarations -> Bool -> Bool -> Expr -> Expr
rewriteExpr globals locals inConstructor bRestrict e = 
    case e of 
        Lit l -> liftExpr globals locals inConstructor bRestrict e
        Var n -> let e' = rewriteVar' n
                 in if isConstructorName n 
                    then    if inConstructor
                            then mkVar $ innerName n
                            else liftExpr globals locals inConstructor bRestrict e'
                    else if bRestrict 
                         then mkRestrictExpr e'
                         else e' 
            {-if isConstructor e then rewriteConstructor globals locals inConstructor e True else-}  -- rewriteVar globals locals inConstructor n 
        -- assuming all infix operators have been lifted, either in 
        -- VPrelude or in the module being lifted
        InfixApp arg1 op arg2 -> 
            let e' = mkInfixApp (rewriteExpr globals locals inConstructor bRestrict arg1) op
                                (rewriteExpr globals locals inConstructor bRestrict arg2) --rewriteInfixApp globals locals inConstructor arg1 op arg2
            in  if bRestrict
                then mkApp toSubV (mkParen e') 
                else e'
        PrefixApp op arg -> mkApp liftedNeg (rewriteExpr globals locals inConstructor bRestrict arg)
        App fun arg ->  let inCons = isConstructor fun
                            fun' = 
                                case fun of
                                    Var _   -> fun
                                    _       -> rewriteExpr globals locals inCons bRestrict fun
                            arg' = rewriteExpr globals locals inCons bRestrict arg
                            e'   = mkApp fun' arg'
                            e''  =  if inCons
                                    then if bRestrict
                                         then mkLiftedExpr e'
                                         else liftExpr globals locals inConstructor False e'
                                    else e' 
                        in  {-if inCons then rewriteConstructor {-globals locals inConstructor e False-} e' else-} e''
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
                        (mkApp  liftedCond  (mkParen (rewriteExpr globals locals inConstructor bRestrict c)))
                        (mkParen $ mkLambda [cntxtPat] (rewriteBranch globals locals bRestrict t)))
                        (mkParen $ mkLambda [cntxtPat] (rewriteBranch globals locals bRestrict e))
        Case v alts -> 
            let e' = rewriteCase globals locals v (_annListElems alts) 
            in  if bRestrict
                then mkApp toSubV (mkParen e')
                else e'
            {-
            if isDeepExpr v then mkCase v $ map (rewriteAlt' globals locals inConstructor) $ _annListElems alts
            else
            let dummy    = mkName dummyVar
                arg      = mkVarPat $ dummy
                splitter = mkParen $ mkLambda [arg] (mkCase (mkVar dummy) (splitAlts 0 $ _annListElems alts))
                as       = map (rewriteAlt globals locals) $ (zip [0..] $ _annListElems alts)
                v'       = mkParen (rewriteExpr globals locals inConstructor v)
            in  mkLet (map (\(a,i) -> mkAltBinding globals locals a i) (zip [0..] (_annListElems alts)))
                      (mkApp (mkApp (mkApp liftedCase v') splitter) (mkList as))
            -}
        MultiIf alts -> trace "Unhandled MultiIf" e 
        Lambda b e -> let vbs = foldr S.union S.empty (map getPatternVars (_annListElems b))
                          locals' = S.union locals vbs 
                      in mkLambda (_annListElems b) (rewriteExpr globals locals' inConstructor bRestrict e)
        Let bs e -> 
            let bs' = foldl (\xs b -> 
                                let   ls =  if null xs 
                                            then locals 
                                            else S.union locals (snd (head xs))
                                in    (rewriteLocalBind globals ls inConstructor bRestrict b) : xs) 
                            [] $ _annListElems bs
                ls  = snd (head bs')
                e'  = rewriteExpr globals ls inConstructor bRestrict e
            in  mkLet ((reverse . fst . unzip) bs') e'
        Do ss -> trace "Unhandled Do" e
        Tuple es -> trace "Unhandled Tuple" e 
        UnboxedTuple es -> trace "Unhandled UnboxedTuple" e 
        TupleSection es -> trace "Unhandled TupleSelection" e 
        UnboxedTupleSection es -> trace "Unhandled UnboxedTupSec" e 
        --List es -> mkApp mkVarT (mkList $ map (rewriteExpr globals locals) (_annListElems es))
        List es -> mkList (map (rewriteExpr globals locals inConstructor bRestrict) (_annListElems es)) -- liftExpr globals locals inConstructor e
        ParArray es -> trace "Unhandled ParArray" e
        Paren ex -> mkParen (rewriteExpr globals locals inConstructor bRestrict ex)
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
