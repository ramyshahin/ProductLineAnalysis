module Rewrite.Pattern where

import Rewrite.Base

import qualified Data.Set as S 

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
        WildPat                 -> S.empty
        _annListElems           -> trace ("Unhandled Pattern " ++ prettyPrint p) $ S.empty
    where unionPatterns ps = foldl S.union S.empty $ map getPatternVars (_annListElems ps)

rewriteCasePattern :: Pattern -> Pattern
rewriteCasePattern p = mkParenPat $ 
    case p of
        VarPat n      -> mkVarPat $ innerName n
        AppPat c args -> mkAppPat (innerName c) (_annListElems args)
        _             -> notSupported p

getPatternName :: Pattern -> Name
getPatternName p =
    case p of
        VarPat n -> n
        AppPat n _ -> n
        _          -> notSupported (mkName "")

rewritePattern :: Pattern -> Pattern
rewritePattern p = 
    case p of
        InfixAppPat lhs o@(NormalOp op) rhs -> 
            let lhs' = rewritePattern lhs
                rhs' = rewritePattern rhs
                op'  = if isPrimitiveOp (prettyPrint op)
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