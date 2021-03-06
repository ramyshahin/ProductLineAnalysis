module Rewrite.Decl where

import Rewrite.Common
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Ann 
import Language.Haskell.Tools.Rewrite.Match.Decls

import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace
import Data.Char
import qualified Data.Set as S 
import qualified SPL as L

import Rewrite.Base

{-
getDeclaredName :: Decl -> String
getDeclaredName d = 
     case d of
        TypeSigDecl sig -> ""
        ValueBinding vb -> [rewriteValueBind globals vb]
        DataDecl newType ctxt hd cns drv -> 
            let newDeclHead = rewriteDeclHead globals hd 
                consNames   = map getConName (_annListElems cns) in 
            [mkDataDecl newType (_annMaybe ctxt) newDeclHead
                        (map (rewriteConDecl globals hd) (_annListElems cns)) 
                        (_annListElems drv),
            -- workaround because mkTypeDecl is buggy
            --mkTypeDecl hd (mkTypeApp tyVar (mkVarType (getName newDeclHead)))
            mkValueBinding $ mkFunctionBind 
                [mkMatch (mkMatchLhs (mkName "type") [mkVarPat $ mkName $ getTypeName hd]) 
                         (mkUnguardedRhs $ (mkApp (mkVar $ mkName "Var") (mkVar $ mkName $ getTypeName newDeclHead))) Nothing]
            ] ++ map liftConstructor consNames
        -- TODO: other cases
        _ -> [notSupported d]
-}

-- | Rewrite declarations
--
isTypeVar :: Name -> Bool
isTypeVar n = isLower $ head $ prettyPrint n

rewriteType :: Declarations -> Type -> Type
rewriteType globals t = case t of
    FunctionType a b    -> 
        mkFunctionType (rewriteType globals a) (rewriteType globals b)
    ParenType t         -> mkParenType (rewriteType globals t)
    TupleType ts        -> 
        mkTupleType (map (rewriteType globals) (_annListElems ts))
    ListType t          -> mkListType (rewriteType globals t)
    VarType  n          -> 
        if isTypeVar n then mkParenType $ mkTypeApp tyVar t
        else mkVarType (liftedTypeName n)
                --if S.member (prettyPrint n) globals 
                --then mkVarType (innerName n)  
                --else mkParenType $ mkTypeApp tyVar t
    TypeApp t1 t2       -> 
        mkTypeApp (rewriteType globals t1) t2
    -- TODO: handle other cases
    _ -> notSupported t

{-
getHeadTypeName :: Type -> Name
getHeadTypeName t =
    case t of
        VarType n -> n
        TypeApp t1 t2 -> getHeadTypeName t1
        _ -> mkName ""
-}

-- TODO
-- mkTypeSignature takes only one name, so a signature might map
-- to multiple declarations
rewriteTypeSig :: Declarations -> TypeSignature -> Decl
rewriteTypeSig globals (TypeSignature ns t) = 
    let n = head $ _annListElems ns
    in  mkTypeSigDecl $ mkTypeSignature n (rewriteType globals t)

recursive :: DeclHead -> Type -> Bool
recursive hd t =
    getTypeName' False True hd == prettyPrint t

{-
typeToLiftedType :: Type -> Type
typeToLiftedType t =
    case t of
        ParenType t         -> mkParenType (typeToLiftedType t)
        VarType   n         -> mkVarType $
            if isTypeVar n 
            then n 
            else liftedTypeName n
        TypeApp t1 t2       -> mkTypeApp (typeToLiftedType t1) (typeToLiftedType t2)
        --TupleType ts        -> mkTupleType (map rewriteType (_annListElems ts))
        --ListType t          -> mkListType (rewriteType t)
        -- TODO: handle other cases
        _ -> notSupported t
-}

renameDeclHead :: DeclHead -> Name -> DeclHead
renameDeclHead dh n =
    case dh of
        NameDeclHead _   -> mkNameDeclHead n
        ParenDeclHead  b -> mkParenDeclHead (renameDeclHead b n)
        DeclHeadApp f op -> mkDeclHeadApp (renameDeclHead f n) op
        InfixDeclHead l op r -> notSupported dh

getTypeName :: Type -> Name
getTypeName t =
    case t of
        VarType n -> n
        TypeApp t1 t2 -> getTypeName t1
        ParenType t -> getTypeName t
        _ -> notSupported $ mkName "getTypeName"

cons2innerType :: Declarations -> DeclHead -> ConDecl -> ((Decl, Decl), (Name, DeclHead))
cons2innerType globals dh c = 
    case c of
        ConDecl n ts ->
            let name     = innerName n 
                ts'      = _annListElems ts
                ts''     = map (rewriteType globals) ts'
                newCons  = mkConDecl name ts'' 
                declHead = renameDeclHead dh name 
                dObj     = mkDefObj (defaultName n) (innerName n) (map getTypeName ts'')
            in  ((mkDataDecl mkDataKeyword Nothing declHead [newCons] [], dObj), (n,declHead)) 

emptyVar :: Name -> Expr
emptyVar n = 
    if   isTypeVar n 
    then mkParen $ mkApp (mkVar $ mkName "Var") (mkList [])
    else mkVar $ defaultName n

mkDefObj :: Name -> Name -> [Name] -> Decl
mkDefObj objName consName args =
    let --objName = defaultName consName
        cons    = mkVar consName
        args'   = map emptyVar args
    in  mkValueBinding $
            mkSimpleBind (mkVarPat objName) (mkUnguardedRhs $ foldl mkApp cons args') Nothing

mkProdCons :: DeclHead -> [DeclHead] -> ConDecl
mkProdCons dh dhs =
    let toField dh = let x = getTypeName' False False dh
                     in  mkName $ 'f' : tail x
        toType  = mkVarType . mkName . (getTypeName' False True)
        tname  = getTypeName' True False dh
        fields = map (\dh -> mkFieldDecl [toField dh] $ toType dh) dhs
    in mkRecordConDecl (mkName tname) fields

-- rewrite constructor declaration
rewriteConDecl :: Declarations -> DeclHead -> ConDecl -> ConDecl
rewriteConDecl globals hd d = 
    case d of
        ConDecl n ts -> 
            mkConDecl n $ 
                (map (\t -> if recursive hd t 
                            then t -- typeToLiftedType t 
                            else 
                                rewriteType globals t)
                     (_annListElems ts))
        _ -> notSupported d

rewriteDeclHead :: Declarations -> DeclHead -> DeclHead
rewriteDeclHead decls dh =
    case dh of
        NameDeclHead n -> mkNameDeclHead (liftedTypeName n)
        ParenDeclHead  b -> mkParenDeclHead (rewriteDeclHead decls b)
        DeclHeadApp f op -> mkDeclHeadApp (rewriteDeclHead decls f) op
        InfixDeclHead l op r -> notSupported dh

getTypeName' :: Bool -> Bool -> DeclHead -> String
getTypeName' lifted full dh =
    case dh of
        NameDeclHead n -> prettyPrint n ++ 
                          if lifted && not (isTypeVar n) then "_" else ""
        ParenDeclHead  b -> getTypeName' lifted full b
        DeclHeadApp f op -> 
            if full 
            then "(" ++ (getTypeName' lifted full f) ++ " " ++ (prettyPrint op) ++ ")"
            else getTypeName' lifted full f
        InfixDeclHead l op r -> ""

getConName :: ConDecl -> Name
getConName c =
    case c of
        ConDecl n ts -> n
        _ -> notSupported (mkName "")

liftConstructor :: Name -> [ConDecl] -> (ConDecl, Int) -> Decl
liftConstructor tname conss (cdecl, index) =
    let (name, args) = case cdecl of 
                            ConDecl n ts -> (n, _annListElems ts)
                            _ -> (mkName "", [])
        argCount     = length args
        consCount    = length conss
        argList      = map (\i -> mkVar $ mkName $ "v" ++ show i) [0..argCount-1]
        argListP     = map (\i -> mkVarPat $ mkName $ "v" ++ show i) [0..argCount-1]
        consArgs     = mkParen $ foldl mkApp (mkVar $ innerName name) argList
        def i        = mkVar $ defaultName $ getConName (conss !! i)
        allArgs      = map (\i -> if i == index then consArgs else def i) [0..consCount-1]
    in mkValueBinding $ mkFunctionBind 
        [mkMatch (mkMatchLhs (consName name) argListP) 
          (mkUnguardedRhs $ foldl mkApp (mkVar (liftedTypeName tname)) allArgs)
          Nothing] 