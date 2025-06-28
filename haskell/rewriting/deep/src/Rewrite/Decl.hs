module Rewrite.Decl where

import Rewrite.Expr 
import Rewrite.Match
import Rewrite.Pattern
import Rewrite.ValueBind

import Control.Reference -- ((.-), (.=), (^.) (&))
import FastString
import Debug.Trace
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as S 
import qualified SPL as L

import Rewrite.Base

getLHSName :: Decl -> Name
getLHSName d =
    case d of
        ValueBinding vb -> getBindLHSName vb
        _ -> trace ("Decl empty: " ++ prettyPrint d) $ mkName ""
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
--isTypeVar :: Name -> Bool
--isTypeVar n = isLower $ head $ prettyPrint n

rewriteType :: Type -> Type
rewriteType t = case t of
    -- arrow types (e.g., Int -> Int)
    FunctionType a b    -> 
        mkFunctionType (rewriteType a) (rewriteType b)
    -- parenthesized type application (e.g., (Int), Maybe (Maybe Int))
    ParenType t         -> mkParenType (rewriteType t)
    -- tuple notation (e.g., (Int, Int))
    TupleType ts        -> 
        mkTupleType (map rewriteType (_annListElems ts))
    -- list notation (e.g., [Int])
    -- TODO: we only lift the element type for now
    ListType t          -> mkListType (rewriteType t)
    -- first-order types (e.g., Int, String)
    VarType  n          -> mkVarType (liftedTypeName n)
    TypeApp t1 t2       -> mkParenType $
        mkTypeApp (rewriteType t1) (rewriteType t2)
    -- TODO: handle other cases
    _ -> notSupported "rewriteType" t

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
    let n       = head $ _annListElems ns
        t'      = rewriteType t
        ctxt    = mkTypeClassContext t
        sig     = mkTypeSignature n $ 
            case ctxt of 
                Nothing -> t'
                Just c  -> mkCtxType c t'
    in  mkTypeSigDecl sig
        
{-
recursive :: DeclHead -> Type -> Bool
recursive hd t =
    getTypeName' False True hd == prettyPrint t
-}
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
        InfixDeclHead l op r -> notSupported "renameDeclHead" dh

getTypeName :: Type -> Name
getTypeName t =
    case t of
        VarType n -> n
        TypeApp t1 t2 -> getTypeName t1
        ParenType t -> getTypeName t
        _ -> notSupported "getTypeName" $ mkName "getTypeName"

getTypeVars :: Name -> [Name]
getTypeVars n =
    let ts = map mkName $ splitOneOf " \t\n()" (prettyPrint n)
    in  filter isTypeVar ts

getTypeVars' :: Type -> [Name]
getTypeVars' t = nubBy (\x y -> prettyPrint x == prettyPrint y) $
    case t of
        VarType n -> if isTypeVar n then [n] else []
        FunctionType a b -> getTypeVars' a ++ getTypeVars' b
        TypeApp t1 t2 -> getTypeVars' t1 ++ getTypeVars' t2
        ParenType t -> getTypeVars' t 
        _ -> notSupported' "getTypeVars'" t []

cons2innerType :: Declarations -> DeclHead -> Name -> ConDecl -> ((Decl, Decl), Decl, (Name, DeclHead))
cons2innerType globals dh tn c = 
    case c of
        ConDecl n ts ->
            let name     = innerName n 
                ts'      = _annListElems ts
                ts''     = map rewriteType ts'
                tNames   = map getTypeName ts''
                newCons  = mkConDecl name ts'' 
                declHead = renameDeclHead dh name 
                fullTypename = mkName $ getTypeName' False True declHead
                dObj     = mkInnerCons n (defaultName tn) --(map getTypeName ts'')
                vclassInst = mkVClassInst (getType declHead) name tNames True False
            in  ((mkDataDecl mkDataKeyword Nothing declHead [newCons] [], dObj), vclassInst, (n,declHead)) 

mkInnerType :: Declarations -> DeclHead -> Name -> [ConDecl] -> (Decl, Name) --((Decl, Decl), Decl, (Name, DeclHead))
mkInnerType globals dh tn' cs = 
    let tn = innerName tn'
        declHead = renameDeclHead dh tn
        cs' = map (\c -> case c of
                    ConDecl n ts ->
                        let name     = innerName n 
                            ts'      = _annListElems ts
                            ts''     = map rewriteType ts'
                            tNames   = map getTypeName ts''
                            newCons  = mkConDecl name ts'' 
                            fullTypename = mkName $ getTypeName' False True declHead
                            dObj     = mkInnerCons n (defaultName tn) --(map getTypeName ts'')
                            vclassInst = mkVClassInst (getType declHead) name tNames True False
                        in newCons
                    ) cs
            in  (mkDataDecl mkDataKeyword Nothing declHead cs' [], tn) --, dObj), vclassInst, (n,declHead)) 
{-
emptyVar :: Name -> Expr
emptyVar n = 
    if   isTypeVar n 
    then mkParen $ mkApp (mkVar $ mkName "Var") (mkList [])
    else mkVar $ defaultName n
-}

mkDefObj :: Name -> Int -> Decl
mkDefObj typeName consCount =
    let objName = defaultName typeName
        cons    = (mkVar . mkName) $ consNameSOP (prettyPrint typeName)
        args'   = map mkVar $ replicate consCount absentCons
    in  mkValueBinding $
            mkSimpleBind (mkVarPat objName) (mkUnguardedRhs $ foldl mkApp cons args') Nothing

mkInnerCons :: Name -> Name -> Decl
mkInnerCons n defObjName = --params =
    let typeName = innerName n
        paramXname = mkName "x"
        paramRname = mkName "r"
        paramX = mkVarPat $ paramXname
        paramR = mkVarPat $ paramRname
        --consCount = length params
        cName = consFnName typeName
        fieldUpdates = [mkFieldUpdate (attributeName n) (mkVar $ paramXname)]
        recUpdate = mkRecUpdate cons fieldUpdates 
        cons    = mkVar paramRname --(mkVar . mkName) $ consNameSOP (prettyPrint typeName)
        --args'   = map mkVar $ replicate consCount absentCons
    in  mkValueBinding $
            mkSimpleBind (mkAppPat cName [paramX, paramR]) (mkUnguardedRhs $ recUpdate) Nothing

{-
mkProdCons :: DeclHead -> [DeclHead] -> Bool -> ConDecl
mkProdCons dh dhs' recursive =
    let dhs = if recursive then (mkDeclHeadApp (mkNameDeclHead (innerName (proxyName ""))) (mkTypeVar vtname')) : dhs' else dhs'
        toField dh = let x = getTypeName' False False dh
                         n = if isProxyType x then x ++ (prettyPrint vtname) else x 
                     in  getFieldForType n
        toType  = (mkTypeApp sumOption) . getType --mkVarType . mkName . (getTypeName' False True)
        vtname' = mkName (getTypeName' True True dh)
        vtname = liftedTypeName (mkName (getTypeName' False False dh))
        tname  = (consNameSOP . prettyPrint) vtname
        fields = map (\dh -> mkFieldDecl [toField dh] $ toType dh) dhs
    in mkRecordConDecl (mkName tname) fields
-}

-- rewrite constructor declaration
rewriteConDecl :: Declarations -> DeclHead -> ConDecl -> ConDecl
rewriteConDecl globals hd d = 
    case d of
        ConDecl n ts -> 
            mkConDecl n $ 
                (map (\t -> --if recursive hd t 
                            --then t -- typeToLiftedType t 
                            --else 
                                rewriteType t)
                     (_annListElems ts))
        _ -> notSupported "rewriteConDecl" d

rewriteDeclHead :: Declarations -> DeclHead -> DeclHead
rewriteDeclHead decls dh =
    case dh of
        NameDeclHead n -> mkNameDeclHead (liftedTypeName n)
        ParenDeclHead  b -> mkParenDeclHead (rewriteDeclHead decls b)
        DeclHeadApp f op -> mkDeclHeadApp (rewriteDeclHead decls f) op -- TODO: rewrite op?
        {-
        InfixDeclHead l op r -> notSupported dh
        -}
        _ -> notSupported "rewriteDeclHead" dh

getType :: DeclHead -> Type 
getType dh = 
    case dh of
        NameDeclHead n -> mkVarType n
        ParenDeclHead b -> mkParenType (getType b)
        DeclHeadApp f op -> mkParenType $ mkTypeApp (getType f) (mkVarType ((mkName . prettyPrint) op))
        _ -> notSupported' "getType" dh (mkVarType (mkName ""))

eqType :: Type -> Type -> Bool
eqType t1 t2 = 
    case (t1, t2) of
        (VarType m, VarType n) -> (isTypeVar m && isTypeVar n) || (prettyPrint m == prettyPrint n) 
        (TypeApp t1 t2, TypeApp t3 t4) -> eqType t1 t3 && eqType t2 t4 
        (ParenType t, t') -> eqType t t'
        (t', ParenType t) -> eqType t' t  
        _ -> False

isRecursiveCons :: DeclHead -> ConDecl -> Bool 
isRecursiveCons dh cd =
    case cd of
        ConDecl _ ts -> any (eqType (getType dh)) (_annListElems ts)

isRecursive :: Decl -> Bool
isRecursive d =
    case d of 
        DataDecl _ _ hd cns _ -> any (isRecursiveCons hd) (_annListElems cns)
        _ -> False

getTypeName' :: Bool -> Bool -> DeclHead -> String
getTypeName' lifted full dh =
    case dh of
        NameDeclHead n -> prettyPrint $ 
                          if lifted && not (isTypeVar n) then liftedTypeName n else n 
        ParenDeclHead  b -> getTypeName' lifted full b
        DeclHeadApp f op -> 
            if full 
            then let r = (getTypeName' lifted full f) ++ " " ++ (prettyPrint op) 
                 in  if lifted then "(" ++ r ++ ")" else r
            else getTypeName' lifted full f
        InfixDeclHead l op r -> ""
{-
getConName :: ConDecl -> Name
getConName c =
    case c of
        ConDecl n ts -> n
        _ -> notSupported (mkName "")
-}
{-
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
-}
rewriteValueBind :: Declarations -> ValueBind -> Decl
rewriteValueBind globals vb = mkValueBinding $ case vb of
    SimpleBind p rhs bs -> 
        let locals = getPatternVars p
        in  mkSimpleBind p (rewriteRhs globals locals False False rhs) (_annMaybe bs)
    FunctionBind ms -> 
        mkFunctionBind (
            map (\m -> rewriteMatch globals (getMatchVars m) False m) (_annListElems ms)) 
    _ -> notSupported "rewriteValueBind" vb --trace ("Unhandled Value Bind " ++ prettyPrint vb) $ vb

isCompType :: Type -> Bool
isCompType t =
    case t of
        TypeApp t1 t2 -> True
        ParenType t -> isCompType t 
        _ -> False

mkTypeClassContext :: Type -> Maybe Context
mkTypeClassContext t =
    let varTypes = map mkVarType (getTypeVars' t)
    in  if length varTypes == 0 
        then Nothing 
        else (Just . mkContext) $ mkClassAssert vclassName varTypes 
    
mkVClassInst :: Type -> Name -> [Name] -> Bool -> Bool -> Decl
mkVClassInst t consName names' inner recursive = 
    let paramCount = if recursive then 1 + length names' else length names'
        names   = if inner then map (\n -> mkName $ show n) [1..(length names')] else 
                  if recursive then proxyName tn : names' else names'
        cons    = mkVar consName
        nilBind = (let objName = mkName "nil"
                       args'   = map mkVar $ replicate paramCount objName
                   in  mkInstanceBind $
                        mkSimpleBind (mkVarPat objName) (mkUnguardedRhs $ foldl mkApp cons args') Nothing)
        combBind = (let v s n = mkVar $ mkName (s ++ (prettyPrint n))
                        comb = mkVar (mkName "comb")
                        arg n s = mkParen $ if inner 
                                  then mkParen $ mkApp (mkApp comb (v "a" n)) (v "b" n) 
                                  else mkApp ((mkVar . attributeName) n) ((mkVar . mkName) s)
                        args = map (\n ->  if inner then arg n "" else mkParen $ mkApp (mkApp comb (arg n "a")) (arg n "b")) names
                        pat s = if inner 
                                then (if length names > 0 then mkParenPat else id) $ 
                                        mkAppPat consName (map (mkVarPat . (\n -> mkName $ s ++ n) . prettyPrint) names) 
                                else mkVarPat $ mkName s 
                        --cons = (mkVar . mkName) $ consNameSOP (prettyPrint tname)
                    in  mkInstanceBind $ mkSimpleBind 
                            (mkAppPat (mkName "comb") [pat "a", pat "b"]) 
                            (mkUnguardedRhs $ foldl mkApp cons args) Nothing)
        proxyBind = mkInstanceBind $ mkSimpleBind 
                            (mkVarPat (mkName "proxy"))
                            (mkUnguardedRhs $ mkInfixApp (mkVar (mkName "resolveVProxy")) compOp (mkVar $ (getFieldForType . prettyPrint . innerName) (proxyName tn))) 
                            Nothing
        --vtype = t --mkVarType fulltname
        --varTypes = map mkVarType (getTypeVars' t)
        --ctxt = if length varTypes == 0 then Nothing else (Just . mkContext) $ mkClassAssert vclassName varTypes 
        ctxt = mkTypeClassContext t
        t'   = if inner then t else rewriteType t
        tn   = (prettyPrint . getTypeName) t'
    in mkInstanceDecl Nothing 
        (mkInstanceRule ctxt (mkAppInstanceHead (mkInstanceHead vclassName) t'))
        (Just $ mkInstanceBody ([nilBind, combBind] ++ if recursive then [proxyBind] else [])) 

proxyCns :: Type -> ConDecl
proxyCns t = 
    let n = getTypeName t
    in  mkConDecl (mkName $ ("Proxy_" ++ prettyPrint n)) [t]

mkVType :: Type -> Name -> Name -> Decl
mkVType t tname innerName = 
    let tvars = getTypeVars' t
        fullLHSType = map mkVarPat (tname : tvars) 
        fullRHSType = foldl mkApp (mkVar innerName) (map mkVar tvars)
    in  mkValueBinding $ mkFunctionBind 
                    [mkMatch (mkMatchLhs (mkName "type") fullLHSType) 
                         (mkUnguardedRhs $ (mkApp (mkVar $ mkName "V") (if tvars == [] then fullRHSType else mkParen fullRHSType))) Nothing]

-- data VLList a = VLList_PoS { 
--    f_Proxy_LList :: SumOption (I_Proxy_LList a), 
--    f_NNil :: SumOption (I_NNil a), 
--    f_CCons :: SumOption (I_CCons a) }
rewriteDecl :: Declarations -> Decl -> [Decl]
rewriteDecl globals d = 
     case d of
        TypeSigDecl sig -> [rewriteTypeSig globals sig]
        ValueBinding vb -> [rewriteValueBind globals vb]
        DataDecl newType ctxt hd cns drv -> 
            let newDeclHead = rewriteDeclHead globals hd
                otname       = mkName $ getTypeName' False False hd 
                oftname      = mkName $ getTypeName' False True hd 
                tname        = mkName $ getTypeName' True False hd
                tname'       = mkName $ getTypeName' True True hd
                --tname'      = liftedTypeName tname
                cns'        = --if isRecursive d
                              --then (proxyCns (getType hd)) : _annListElems cns
                              --else 
                                _annListElems cns
                --conss       = length cns'
                --consNames   = map getConName (_annListElems cns) 
                recursive   = isRecursive d
                (innerType, innerName)   = mkInnerType globals hd otname cns'
                --(innerTypes', vclassInsts, dhs') = unzip3 $ map (cons2innerType globals hd tname') cns'
                --(innerTypes, defObjs) = unzip innerTypes'
                --inner = head innerTypes
                --(names,dhss)= unzip dhs'
                --prodCons    = mkProdCons hd dhss recursive -- map (mkName . (getTypeName False True)) dhs -- (_annListElems cns)
                --liftdConss  = map (rewriteConDecl globals hd) cns'
                --def         = mkDefObj tname' (length cns') 
                --vclassInst  = mkVClassInst (getType hd) ((mkName . consNameSOP . prettyPrint) tname) names False recursive
                -- workaround because mkTypeDecl is buggy
                --mkTypeDecl hd (mkTypeApp tyVar (mkVarType (getName newDeclHead)))
                vt = mkVType (getType hd) tname innerName
                --vt          = mkTypeDecl hd (mkTypeApp tyVar (mkVarType innerName))
            in  [innerType, vt]
                --[mkDataDecl newType (_annMaybe ctxt) newDeclHead --liftdConss
                    --[prodCons]
                --    []
                --    (_annListElems drv)
                    --, vclassInst
                --    ] ++ 
                    --[mkTypeDecl (mkVarT innerType) (mkVarType innerType)] -- ++
                    --vclassInsts ++
                    --innerTypes ++ 
                    --defObjs -- ++ 
                    --map (liftConstructor tname cns') (zip cns' [0..])
        _ -> [notSupported "rewriteDecl" d]
