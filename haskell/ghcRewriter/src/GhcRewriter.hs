module GhcRewriter(plugin) where

import GHC
import GhcPlugins
import HsSyn
import HsTypes
import CoreSyn
import Unique 
import Data.List(isPrefixOf)
import Module
import GHC.Paths
import OccName
import Debug

plugin :: Plugin
plugin = defaultPlugin {
  --installCoreToDos = coreToDos,
  parsedResultAction = modifyPreamble
  }
  
modifyPreamble :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
modifyPreamble _ _ m = 
  (renameModule m) >>= 
  addDependencies >>= rewriteBindings >>= dumpMod

showO prefix o = do
  flags <- getDynFlags
  liftIO $ putStrLn $ prefix ++ showSDoc flags (ppr o)

showO2 prefix o1 o2 = do
    flags <- getDynFlags
    liftIO $ putStrLn $ prefix ++ "\t" ++ showSDoc flags (ppr o1) ++ "\t" ++
                                  showSDoc flags (ppr o2)

idVar :: RdrName
idVar = mkUnqual OccName.tvName (fsLit "Var")

typeVar :: LHsType GhcPs
typeVar = L noSrcSpan (HsTyVar NoExt NotPromoted (L noSrcSpan idVar))

idMakeVarT :: RdrName
idMakeVarT = mkUnqual OccName.varName (fsLit "mkVarT")

exprMakeVarT :: LHsExpr GhcPs
exprMakeVarT = L noSrcSpan (HsVar NoExt (L noSrcSpan idMakeVarT))

rewriteType :: LHsType GhcPs -> Hsc (LHsType GhcPs)
rewriteType t = do {-
  let L _ t' = t
  case t' of
    HsForAllTy a b c -> showO2 "HsForAllTy" a c
    HsQualTy a b c -> showO2 "HsQualTy" a c
    HsTyVar a b c -> showO2 "HsTyVar" a c
    HsAppTy a b c -> do
      b' <- rewriteType b
      showO2 "HsAppTy" b c
    HsFunTy a b c -> showO2 "HsFunTy" a c
    HsListTy a b -> showO2 "HsListTy" a b
    HsTupleTy a b c -> showO2 "HsTupleTy" a c
    HsSumTy a b -> showO2 "HsSumTy" a b
    HsOpTy a b c d -> showO2 "HsOpTy" a b
    HsParTy a b -> showO2 "HsParTy" a b
    HsIParamTy a b c -> showO2 "HsIParamTy" a b
    HsStarTy a b -> showO2 "HsStarTy" a b
    HsKindSig a b c -> showO2 "HsKindSig" a b
    HsSpliceTy a b -> showO2 "HsSpliceTy" a b
    HsDocTy a b c -> showO2 "HsDocTy" a b
    HsBangTy a b c -> showO2 "HsBangTy" a b
    HsRecTy a b -> showO2 "HsRecTy" a b
    HsExplicitListTy a b c -> showO2 "HsExplicitListTy" a c
    HsExplicitTupleTy a b -> showO2 "HsExplicitTupleTy" a b
    HsTyLit a b -> showO2 "HsTyLit" a b
    HsWildCardTy a -> showO "HsWildCardTy" a
    XHsType a -> showO "XHsType" a -}
  return $ L noSrcSpan (HsAppTy NoExt typeVar t)

rewriteHsImplicitBndrs :: HsImplicitBndrs GhcPs (LHsType GhcPs)
                       -> Hsc (HsImplicitBndrs GhcPs (LHsType GhcPs))
rewriteHsImplicitBndrs x =
  case x of 
    HsIB a b -> do
      b' <- rewriteType b 
      return $ HsIB a b'
    _ -> return x 

rewriteLHsSigWcType :: HsWildCardBndrs GhcPs (LHsSigType GhcPs)  
                    -> Hsc (HsWildCardBndrs GhcPs (LHsSigType GhcPs)) 
rewriteLHsSigWcType x = 
  case x of
    HsWC a t -> do
      t' <- rewriteHsImplicitBndrs t
      return $ HsWC a t'
    XHsWildCardBndrs a -> showO "XHsWildCardBndrs" a >> return x 

rewriteSig :: Sig GhcPs -> Hsc (Sig GhcPs)
rewriteSig sig =
  case sig of
    TypeSig a b t -> do {t' <- rewriteLHsSigWcType t; return $ TypeSig a b t'}
    PatSynSig a b c -> showO2 "PatSynSig" a c  >> return sig
    ClassOpSig a b c d -> showO2 "ClassOpSig" a d >> return sig 
    IdSig a b -> showO2 "IdSig" a b >> return sig 
    FixSig a b -> showO2 "FixSig" a b >> return sig 
    InlineSig a b c -> showO2 "InlineSig" a b >> return sig 
    SpecSig a b c d -> showO2 "SpecSig" a d >> return sig 
    SpecInstSig a b c -> showO2 "SpecInstSig" a b >> return sig 
    MinimalSig a b c -> showO2 "MinimalSig" a b >> return sig 
    SCCFunSig a b c d -> showO2 "SCCFunSig" a d >> return sig 
    CompleteMatchSig a b c d -> showO2 "CompleteMatchSig" a d >> return sig 
    XSig a -> showO "XSig" a >> return sig 

rewriteExpr :: LHsExpr GhcPs -> Hsc (LHsExpr GhcPs)
rewriteExpr e = do
  showO "Expr: " e 
  let (L s e') = e
  case e' of
    HsOverLit _ _ -> do
      showO "Overloaded Literal:" e'
      return $ L s (HsApp NoExt exprMakeVarT e)
    HsLit _ _ -> do
      showO "Literal:" e' 
      return $ L s (HsApp NoExt exprMakeVarT e)
    _ -> return e 

    
rewriteLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> Hsc (LGRHS GhcPs (LHsExpr GhcPs))
rewriteLGRHS lg = do
  let (L s g) = lg
  case g of
    GRHS x ss e -> do 
      e' <- rewriteExpr e
      return $ L s (GRHS x ss e')
    _ -> return lg

rewriteLHsLocalBinds :: LHsLocalBinds GhcPs -> Hsc (LHsLocalBinds GhcPs)
rewriteLHsLocalBinds lbs = do
  showO "LocalBinds: " lbs
  let (L s bs) = lbs
  case bs of {-
    HsValBinds x binds -> do
      binds' <-
      return $ L s (HsValBinds x binds')
    HsIPBinds x binds -> do
      binds' <- 
      return $ L s (HsIPBinds x binds') -}
    _ -> return lbs

-- rewrite Guarded Right-Hand-Sides
rewriteGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> Hsc (GRHSs GhcPs (LHsExpr GhcPs))
rewriteGRHSs grhss = do
  --showO "GRHSs: " grhss
  case grhss of
    GRHSs ext gs bnds -> do 
      gs' <- mapM rewriteLGRHS gs
      bnds' <- rewriteLHsLocalBinds bnds
      return $ GRHSs ext gs' bnds' 
    _ -> return grhss 

rewritePat :: LPat GhcPs -> Hsc (LPat GhcPs)
rewritePat lp = do
  showO "Pat: " lp
  let (L s p) = lp
  case p of
    _ -> return lp 

rewriteMatch :: LMatch GhcPs (LHsExpr GhcPs) -> Hsc (LMatch GhcPs (LHsExpr GhcPs))
rewriteMatch lm = do
  showO "Match: " lm
  let (L s m) = lm 
  case m of 
    Match e c p g -> do
      p' <- mapM rewritePat p 
      g' <- rewriteGRHSs g 
      return $ L s (Match e c p' g')
    _ -> return lm

rewriteMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
                  -> Hsc (MatchGroup GhcPs (LHsExpr GhcPs))
rewriteMatchGroup mg = do
  --showO "Match Group: " mg 
  case mg of
    MG ext (L s alts) origin -> do 
      alts' <- mapM rewriteMatch alts
      return $ MG ext (L s alts') origin
    _ -> return mg

rewriteBind :: HsBind GhcPs -> Hsc (HsBind GhcPs)
rewriteBind b = do
  showO "Bind: " b
  case b of
    FunBind ext id mg co_fn tick ->
      do
        mg' <- rewriteMatchGroup mg
        return $ FunBind ext id mg' co_fn tick
    VarBind ext id rhs inline -> do
      rhs' <- rewriteExpr rhs
      return $ VarBind ext id rhs' inline
    _ -> return b  

rewriteDecl' :: HsDecl GhcPs -> Hsc (HsDecl GhcPs)
rewriteDecl' decl = do
  case decl of 
    TyClD t d -> showO2 "TyClD" t d >> return decl 
    InstD a b -> showO2 "InstD" a b >> return decl 
    DerivD a b -> showO2 "DerivD" a b >> return decl 
    ValD a b -> do 
      b' <- rewriteBind b
      return $ ValD a b' 
    SigD a b -> do
      b' <- rewriteSig b
      return $ SigD a b'
    _ -> showO "" decl >> return decl 

rewriteDecl :: LHsDecl GhcPs -> Hsc (LHsDecl GhcPs)
rewriteDecl decl = do
  let (L s d) = decl
  d' <- rewriteDecl' d
  return (L s d')

rewriteBindings :: HsParsedModule -> Hsc HsParsedModule
rewriteBindings m = do
  let hsMod@(L s mod)       = hpm_module m
  let decls = hsmodDecls mod
  decls' <- mapM rewriteDecl decls
  let mod' = mod{hsmodDecls = decls'}
  return $ m { hpm_module = L s mod' } 

coreToDos :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreToDos _ todo = do
  putMsgS "Deep lifting plugin"
  return ((CoreDoPluginPass "Deep lifting" pass) : todo)

dumpMod :: HsParsedModule -> Hsc HsParsedModule
dumpMod m = do
  showO "" (hpm_module m)
  return m 

-- does this actually rename the module? do we also need to rename the source file?
renameModule :: HsParsedModule -> Hsc HsParsedModule
renameModule m =
  let hsMod@(L s mod)       = hpm_module m
      (Just (L x modName))  = hsmodName mod
      name  flags           = showSDoc flags (ppr modName)
      name' flags           = mkModuleName $ (name flags) ++ "Deep"
      mod' flags            = m { hpm_module = L s (mod { hsmodName = Just (L x (name' flags))})}
  in  do
    flags <- getDynFlags
    return $ mod' flags

splModuleName = mkModuleName "SPL"

addDependencies :: HsParsedModule -> Hsc HsParsedModule
addDependencies m = 
  let hsMod@(L s mod) = hpm_module m
      imports         = hsmodImports mod
      splImport       = simpleImportDecl splModuleName
      imports'        = (L noSrcSpan splImport) : imports
  in  return $ m { hpm_module = L s (mod { hsmodImports = imports'}) }

-- idMkVarT = mkGlobalVar VanillaId $ mkSystemName initTyVarUnique (mkVarOcc "mkVarT")

liftExpr :: OutputableBndr b => Var -> Expr b -> CoreM (Expr b)
liftExpr v l@(Lit _) = return $ App (Var v) l
liftExpr _ e = return e

liftBind :: Var -> CoreBind -> CoreM CoreBind
liftBind varMkVarT (NonRec v expr) = do
  expr' <- liftExpr varMkVarT expr
  return (NonRec v expr')

liftBind varMkVarT (Rec bs) = do
  bs' <- mapM (\(b, expr) -> do expr' <- liftExpr varMkVarT expr; return (b, expr')) bs
  return (Rec bs')

lookupID flags names id = head $ filter (\n -> (showSDoc flags . ppr) n == id) names

printBind :: Var -> CoreBind -> CoreM CoreBind
printBind varMkVarT bndr@(NonRec b _) = do
  flags <- getDynFlags
  putMsgS $ "Non-recursive binding name " ++ (showSDoc flags (ppr b))
  liftBind varMkVarT bndr
  
printBind _ bndr = return bndr 

pass :: ModGuts -> CoreM ModGuts
pass mod = do
          env   <- getHscEnv
          flags <- getDynFlags
          --splMod <- getUsedModule mod "SPL"
          names <- getModuleNames env (mg_module mod)
          let nameMkVarT = lookupID flags names "mkVarT"
          let [(varMkVarT, _)] = lookupNameInGuts mod nameMkVarT      
          putMsg (ppr varMkVarT)
          mod' <- bindsOnlyPass (mapM (printBind varMkVarT)) mod
          return mod'

