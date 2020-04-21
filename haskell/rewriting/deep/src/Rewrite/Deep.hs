
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
import qualified Data.Set as S 
import qualified SPL as L

type Declarations = S.Set String

deepRewrite :: RefactoringChoice
deepRewrite = ModuleRefactoring "DeepRewrite" (localRefactoring deep)
    
run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . (\_ -> deep))
    
deep :: LocalRefactoring
deep mod = do
    let decls = getModuleDeclarations mod
    return  $ filePragmas & annList     .- rewritePragma 
            $ modHead     & annMaybe    .- rewriteHeader
            $               modImports  .- rewriteImports
            $ modDecl     & annList     .- (rewriteDecl decls)
            $ mod 
    
getBindLHSName :: ValueBind -> Name
getBindLHSName b = 
    case b of 
        SimpleBind (VarPat p) _ _ -> p
        FunctionBind ms ->  let  ms'   = _annListElems ms
                                 h     = head ms'
                                 match = h ^. matchLhs
                            in case match of 
                                MatchLhs n _ -> trace ("Decl: " ++ prettyPrint n) n
                                _ -> mkName ""

getLHSName :: Decl -> Name
getLHSName d =
    case d of
        ValueBinding vb -> getBindLHSName vb
        _ -> trace ("Decl empty: " ++ prettyPrint d) $ mkName ""

getModuleDeclarations :: Module -> Declarations
getModuleDeclarations mod = do
    let decls = _annListElems $ mod ^. modDecl
        names = map (prettyPrint . getLHSName) decls
        s     = S.fromList names
    trace (debugDecls s) $ s

getPatternVars :: Pattern -> S.Set String
getPatternVars p =  
    case p of
        VarPat v                -> S.singleton $ prettyPrint v
        InfixAppPat lhs _ rhs   -> S.union (getPatternVars lhs) (getPatternVars rhs)
        AppPat _ ps             -> unionPatterns ps
        TuplePat ps             -> unionPatterns ps
        ListPat ps              -> unionPatterns ps
        ParenPat p              -> getPatternVars p
        TypeSigPat p _          -> getPatternVars p
        _annListElems           -> trace ("Unhandled Pattern " ++ prettyPrint p) $ S.empty
    where unionPatterns ps = foldl S.union S.empty $ map getPatternVars (_annListElems ps)

getMatchVars :: Match -> S.Set String
getMatchVars (Match lhs _ _) =
    case lhs of 
        MatchLhs _ args -> foldl S.union S.empty $ map getPatternVars (_annListElems args)
        InfixLhs lhs _ rhs args -> 
            foldl S.union (S.union (getPatternVars lhs) (getPatternVars rhs)) $ map getPatternVars (_annListElems args)
{-
getValBindVars :: ValueBind -> [String]
getValBindVars vb =
    case vb of

getLocalVars :: LocalBind -> [String]
getLocalVars b =
    case b of
        LocalValBind vb -> getValBindVars vb
        _               -> trace ("Unhandled LocalBind " ++ prettyPrint b) $ []

getLocalsVars :: LocalBinds -> [String]
getLocalsVars bs = foldl (++) [] $ map getLocalVars (_annListElems bs)
-}

-- | Rewriting imports
--
imports xs = map snd (zipWithSeparators xs)
    
rewritePragma = id

rewriteHeader :: Maybe ModuleHead -> Maybe ModuleHead 
rewriteHeader header =
    case header of
        Nothing -> Nothing
        Just h  -> Just $ mhName .- (appendModName "Deep") $ h

moduleNameDeepTypes = mkModuleName "DeepTypes"
importDeepTypes = mkImportDecl False False False Nothing moduleNameDeepTypes Nothing Nothing

rewriteImports :: ImportDeclList  -> ImportDeclList 
rewriteImports xs = (annListElems .= concat [[importSPL{-, importDeepTypes-}], (imports xs)]) xs 
    
-- | Rewrite declarations
--
rewriteDecl :: Declarations -> Decl -> Decl
rewriteDecl decls d = 
     case d of
        TypeSigDecl sig -> rewriteTypeSig sig
        ValueBinding vb -> rewriteValueBind decls vb
        -- TODO: other cases
        _ -> trace ("Unhandled Decl " ++ prettyPrint d) $ d

rewriteValueBind :: Declarations -> ValueBind -> Decl
rewriteValueBind globals vb = mkValueBinding $ case vb of
    SimpleBind p rhs bs -> 
        let locals = getPatternVars p
        in  mkSimpleBind p (rewriteRhs globals locals rhs) (_annMaybe bs)
    FunctionBind ms -> 
        mkFunctionBind (
            map (\m -> rewriteMatch globals (getMatchVars m) m) (_annListElems ms)) 
    _ -> trace ("Unhandled Value Bind " ++ prettyPrint vb) $ vb

rewriteRhs :: Declarations -> Declarations -> Rhs -> Rhs
rewriteRhs globals locals rhs = case rhs of
    UnguardedRhs e -> mkUnguardedRhs $ rewriteExpr globals locals e
    _              -> trace ("Unhandled RHS " ++ prettyPrint rhs) $ rhs

rewriteMatch :: Declarations -> Declarations -> Match -> Match
rewriteMatch globals locals (Match lhs rhs binds) = 
    mkMatch lhs (rewriteRhs globals locals rhs) (_annMaybe binds)

liftedCond = mkVar (mkName "liftedCond")
liftedNeg = mkVar (mkName "liftedNeg")
liftedCase = mkVar (mkName "liftedCase")

liftOp (NormalOp o) = mkParen (mkApp mkVarT (mkVar (mkParenName o)))

rewriteVar :: Declarations -> Declarations -> Name -> Expr
rewriteVar globals locals vn = 
    if   (externalDecl globals locals vn)
    then mkParen $ mkApp mkVarT (mkVar vn)
    else mkVar vn

debugDecls :: Declarations -> String
debugDecls ns = foldl (\r s -> r ++ " " ++ s) "Declarations: " $ ns

externalDecl :: Declarations -> Declarations -> Name -> Bool
externalDecl globals locals x = 
    let allDecls = S.union globals locals
        r = not $ S.member (prettyPrint x) allDecls
    in  trace (debugDecls allDecls ++ " External " ++ prettyPrint x ++ " ? " ++ show r) $ r

getLiftedPrimitiveOp :: String -> Operator
getLiftedPrimitiveOp o = mkUnqualOp (o ++ "^")

rewriteInfixApp :: Declarations -> Declarations -> Expr -> Operator -> Expr -> Expr
rewriteInfixApp globals locals lhs op rhs =
    let lhs' = rewriteExpr globals locals lhs
        rhs' = rewriteExpr globals locals rhs
        rewriteIt = mkInfixApp    (mkInfixApp (liftOp op) appOp (mkParen lhs')) 
                                        appOp 
                                        (mkParen $ rhs')
    in  case op of
            NormalOp n  -> 
                if      S.member (prettyPrint n) L.primitiveOpNames
                then    mkInfixApp lhs' (getLiftedPrimitiveOp (prettyPrint n)) rhs'
                else    rewriteIt
            _           -> rewriteIt

rewritePattern :: Pattern -> Pattern
rewritePattern p = 
    case p of
        InfixAppPat lhs o@(NormalOp op) rhs -> 
            let lhs' = rewritePattern lhs
                rhs' = rewritePattern rhs
                op'  = if S.member (prettyPrint op) L.primitiveOpNames
                       then getLiftedPrimitiveOp (prettyPrint op)
                       else o
            in mkInfixAppPat lhs' op' rhs'
        ParenPat p' -> mkParenPat $ rewritePattern p'
        {- ListPat ps  ->  let ps' = _annListElems ps
                            op  = getLiftedPrimitiveOp ":"
                        in  mkParenPat (foldr (\l r -> mkInfixAppPat l op r) nil' ps')
        VarPat n    -> trace ("Var pattern: " ++ (prettyPrint p)) p
        LitPat l    -> trace ("Literal pattern: " ++ (prettyPrint p)) p
        AppPat c args -> if (prettyPrint c) == "[]"
                         then mkAppPat nilName (_annListElems args)
                         else p
        TuplePat _  -> trace ("Tuple pattern: " ++ (prettyPrint p)) p -}
        _ -> trace ("Unknown pattern: " ++ (prettyPrint p)) p

rewriteAlt :: Declarations -> Declarations -> (Integer, Alt) -> Expr
rewriteAlt globals locals (index, Alt p (CaseRhs e) _) =
    let vCase   = mkVar $ mkName ("case" ++ show index)
        vSplit  = mkVar $ mkName ("split" ++ show index)
        vLiftV  = mkVar $ mkName ("liftV")
        pVars   = getPatternVars p
        vUncurry = mkVar $ mkName ("uncurry" ++ show (length pVars))
        dotOp   = mkUnqualOp "."
    in  --if (length pVars) > 0 
        {- then -} mkInfixApp (mkParen (mkApp vUncurry vCase)) dotOp (mkParen (mkApp vLiftV vSplit))
        --else vCase

splitAlts :: Integer -> [Alt] -> [Alt]
splitAlts index as =
    case as of
        []                  -> []
        ((Alt p _ _) : as') -> 
            let rhs = mkCaseRhs $ mkLit (mkIntLit index)
            in  (mkAlt p rhs Nothing) : (splitAlts (index + 1) as')

dummyVar = "__dummy__"

mkAltBinding :: Declarations -> Declarations -> Integer -> Alt -> LocalBind
mkAltBinding globals locals index (Alt p (CaseRhs rhs) _) = 
    let lhsName = mkName $ "case" ++ (show index)
        splitName = mkName $ "split" ++ (show index)
        params' = getPatternVars p
        params = S.toList $ params'
        lhs = mkMatchLhs lhsName (map (mkVarPat . mkName) params)
        dummy = mkName dummyVar
        splitLhs = mkMatchLhs splitName [mkVarPat dummy]
        splitAlt = mkAlt p (mkCaseRhs (mkTuple (map (mkVar . mkName) params))) Nothing
    in  mkLocalValBind $ mkFunctionBind [mkMatch lhs (mkUnguardedRhs (rewriteExpr globals (S.union locals params') rhs)) Nothing,
                                         mkMatch splitLhs (mkUnguardedRhs (mkCase (mkVar dummy) [splitAlt])) Nothing]

-- lifting expressions 
rewriteExpr :: Declarations -> Declarations -> Expr -> Expr
rewriteExpr globals locals e = 
    case e of 
        Lit l -> mkParen $ mkApp mkVarT (mkLit l)
        Var n -> rewriteVar globals locals n 
        InfixApp arg1 op arg2 -> rewriteInfixApp globals locals arg1 op arg2
        PrefixApp op arg -> mkInfixApp liftedNeg appOp arg
        App fun arg ->  let fun' = rewriteExpr globals locals fun
                            arg' = rewriteExpr globals locals arg
                        in  case fun of 
                                Var n -> if (externalDecl globals locals n)
                                         then   if S.member (prettyPrint n) L.primitiveFuncNames
                                                then mkApp (mkVar $ mkName ((prettyPrint n) ++ "\'")) arg'
                                                else mkInfixApp fun appOp arg'
                                         else mkApp fun arg'
                                _     -> mkInfixApp fun' appOp arg'
        If c t e -> mkApp   (mkApp  (mkApp  liftedCond  
                                            (mkParen (rewriteExpr globals locals c)))
                                    (mkParen (rewriteExpr globals locals t)))
                            (mkParen (rewriteExpr globals locals e))
        Case v alts -> 
            let dummy    = mkName dummyVar
                arg      = mkVarPat $ dummy
                splitter = mkParen $ mkLambda [arg] (mkCase (mkVar dummy) (splitAlts 0 $ _annListElems alts))
                as       = map (rewriteAlt globals locals) $ (zip [0..] $ _annListElems alts)
            in  mkLet (map (\(a,i) -> mkAltBinding globals locals a i) (zip [0..] (_annListElems alts)))
                      (mkApp (mkApp (mkApp liftedCase v) splitter) (mkList as))
        MultiIf alts -> trace "Unhandled MultiIf" e 
        Lambda b e -> trace "Unhandled Lambda" e
        Let b e -> trace "Unhandled Let" e
        Do ss -> trace "Unhandled Do" e
        Tuple es -> trace "Unhandled Tuple" e 
        UnboxedTuple es -> trace "Unhandled UnboxedTuple" e 
        TupleSection es -> trace "Unhandled TupleSelection" e 
        UnboxedTupleSection es -> trace "Unhandled UnboxedTupSec" e 
        --List es -> mkApp mkVarT (mkList $ map (rewriteExpr globals locals) (_annListElems es))
        List es -> mkApp mkVarT e
        ParArray es -> trace "Unhandled ParArray" e
        Paren ex -> mkParen (rewriteExpr globals locals ex)
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
