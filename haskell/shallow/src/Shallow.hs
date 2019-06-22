module Shallow where

-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 

import Control.Reference ((.-), (.=), (^.))
import SrcLoc (RealSrcSpan, realSrcLocSpan, mkRealSrcLoc)
import FastString

dummySpan :: RealSrcSpan
dummySpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "") 0 0 

shallowRewrite :: RefactoringChoice
shallowRewrite = ModuleRefactoring "ShallowRewrite" (localRefactoring shallow)

run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . (\_ -> shallow))

shallow :: LocalRefactoring
shallow mod = do
    let pragmas = mod ^. filePragmas
    let head = _annMaybe $ mod ^. modHead
    let name = case head of
                    Nothing -> mkModuleName ""
                    Just h -> h ^. mhName
    let mod' = modImports .- (rewriteImports name) $ mod
    let renamedMod = renameModule mod'
    return $ (modDecl .- (annList .- rewriteDecl)) renamedMod

-- | Rewriting imports
--
moduleNameSPL = mkModuleName "SPL"

origAliasStr = "O"
origAlias = mkModuleName origAliasStr

importSPL = mkImportDecl False False False Nothing moduleNameSPL Nothing Nothing
importOrig n = mkImportDecl False True False Nothing n (Just origAlias) Nothing

imports xs = map snd (zipWithSeparators xs)

rewriteImports :: ModuleName -> ImportDeclList  -> ImportDeclList 
rewriteImports n xs = (annListElems .= concat [[(importOrig n), importSPL], (imports xs)]) xs 

-- | Rename module
--
appendName :: String -> ModuleName -> ModuleName
appendName s mn = mkModuleName $ (mn ^. moduleNameString) ++ s

updateHead :: Maybe ModuleHead -> Maybe ModuleHead
updateHead mh =  
    case mh of
        Just mh' -> Just $ (mhName .- (appendName "Shallow")) $ mh'
        _ -> mh

renameModule :: Module -> Module
renameModule mod = (modHead .- (annMaybe .- updateHead)) mod

-- | Rewrite declarations
--
appOp =  mkUnqualOp "<*>"
mkVarT = mkVar (mkName "mkVarT")
tyVar = mkVarType $ mkName "Var"

origName :: Name -> Name
origName n = 
    let sn = semanticsName n 
    in  case sn of 
        Just sn' -> mkNormalName $ mkQualifiedName' [origAliasStr] sn'
        _ -> n

rewriteType :: Type -> Type
rewriteType t = case t of
    FunctionType a b -> mkFunctionType (rewriteType a) (rewriteType b)
    -- TODO: handle other cases
    _ -> mkTypeApp tyVar t

-- TODO
-- mkTypeSignature takes only one name, so a signature might map
-- to multiple declarations
rewriteTypeSig :: TypeSignature -> Decl
rewriteTypeSig (TypeSignature ns t) = 
    let n = head $ _annListElems ns
    in  mkTypeSigDecl $ mkTypeSignature n (rewriteType t)

rewriteVar :: Name -> Expr
rewriteVar vn = mkApp mkVarT (mkVar (origName vn))

pat2expr (VarPat n) = mkVar n 

rewriteArgs :: [Pattern] -> Expr
rewriteArgs ps = 
    case ps of
        p : [] -> pat2expr p 
        p : ps' -> mkInfixApp (pat2expr p) appOp (rewriteArgs ps')

rewriteMatch :: Match -> Match
rewriteMatch (Match lhs rhs binds) = 
    let (MatchLhs lhsName lhsArgs) = lhs
        rhs' = mkUnguardedRhs $ 
                    mkInfixApp  (mkParen (rewriteVar lhsName)) 
                                appOp 
                                (rewriteArgs (_annListElems lhsArgs))
    in  mkMatch lhs rhs' (_annMaybe binds)

rewriteValueBind :: ValueBind -> Decl
rewriteValueBind vb = case vb of
    SimpleBind p rhs ls -> 
        case p of
            VarPat vp -> mkValueBinding $ 
                mkSimpleBind p (mkUnguardedRhs (rewriteVar vp)) (_annMaybe ls)  
            _ -> mkValueBinding vb
    FunctionBind ms -> mkValueBinding $ mkFunctionBind (map rewriteMatch (_annListElems ms)) 

rewriteDecl :: Decl -> Decl
rewriteDecl d = 
    case d of
        TypeSigDecl sig -> rewriteTypeSig sig
        ValueBinding vb -> rewriteValueBind vb
        _ -> d
        -- TODO: other cases

{-    
rewrite :: RealSrcSpan -> LocalRefactoring
rewrite sp = return . --(nodesContained sp .- liftDecl) . 
                      --(nodesContained sp .- liftExpr) .
                       (nodesContained sp .- updateModName) .
                       (nodesContained sp .- updateImports)

updateModName :: ModuleHead -> ModuleHead
updateModName (ModuleHead (ModuleName n) _ _) = 
    mkModuleHead (mkModuleName (n ++ "Shallow")) Nothing Nothing

moduleNameSPL = mkModuleName "SPL"
origModName = mkModuleName "TODO"

importSPL = mkImportDecl False False False Nothing moduleNameSPL Nothing Nothing
importOrig = mkImportDecl False False False Nothing origModName Nothing Nothing

imports xs = map snd (zipWithSeparators xs)

updateImports :: ImportDeclList -> ImportDeclList
updateImports xs = (annListElems .= concat [[importOrig, importSPL], (imports xs)]) xs


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
-}