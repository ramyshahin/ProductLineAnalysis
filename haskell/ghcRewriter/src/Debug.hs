module Debug where

import GhcPlugins
import Avail
import Data.IORef.Lifted (readIORef )
import NameCache (nsNames )
import HscMain
import GHC 
import TcRnTypes (TcGblEnv)

modNameFromUsage :: Usage -> String
modNameFromUsage (UsagePackageModule mod _ _) = (moduleNameString . moduleName) mod
modNameFromUsage (UsageHomeModule modName _ _ _ _) = moduleNameString modName 
modNameFromUsage (UsageMergedRequirement mod _) = (moduleNameString . moduleName) mod
modNameFromUsage _ = ""

moduleFromUsages :: String -> [Usage] -> Maybe Module
moduleFromUsages n us = 
  let rs = filter (\u -> modNameFromUsage u == n) us
  in  case rs of 
        []    -> Nothing
        x : _ -> case x of
                  UsagePackageModule mod _ _    -> Just mod
                  UsageMergedRequirement mod _  -> Just mod
                  _ -> Nothing
                 
--loadSPL :: IO ModGuts
--loadSPL pkgDB = runGhc (Just dir) $ do
--  target <- guessTarget "SPL" Nothing
--  setTargets [target]
--  load LoadAllTargets
--  modSum <- getModSummary $ mkModuleName "B"
  --p <- parseModule modSum
  --t <- typecheckModule p
  --d <- desugarModule t
  --c <- return $ coreModule d
--  return modSum

--typeVar = mkStrLitTy $ fsLit "Var"
--typeVarA = mkForAllTy tyVarA Required typeVar

--loadMod :: HscEnv -> String -> IO ModGuts
--loadMod env modName = do
--    --setSession env
--    summary <- liftIO $ getModSummary $ mkModuleName modName
--    p <- parseModule summary
--    t <- typecheckModule p
--    let tcEnv = (fst . tm_internals_) t
--    liftIO $ hscDesugar env summary tcEnv 

printDependencies mg = do
    let deps = mg_deps mg
    let mods = dep_mods deps
    putMsgS "dep_mods:"
    mapM_ (putMsg . ppr . fst) mods
    let pkgs = dep_pkgs deps
    putMsgS "dep_pkgs:"
    mapM_ (putMsg . ppr . fst) pkgs

getUsedModule mg name = 
    let us = mg_usages mg
        us' = filter (\u -> case u of 
            UsagePackageModule m _ _ -> True
            _ -> False) us
        mods = map (\(UsagePackageModule m _ _) -> m) us'
        matched = filter (\m -> moduleName m == (mkModuleName name)) mods
    in  return $ head matched
        
printUsages mg = do
    putMsgS "Usages:"
    let us = mg_usages mg
    mapM_ (\u -> case u of
        UsagePackageModule m _ _ -> putMsgS "\tUsagePackageModule:" >> putMsg (ppr m)
        UsageHomeModule mn _ _ _ _ -> putMsgS "\tUsageHomeModule:" >> putMsg (ppr mn)
        UsageFile fp _ -> putMsgS $ "\tUsageFile: " ++ fp
        UsageMergedRequirement m _ -> putMsgS "\tUsageMergedRequirement:" >> putMsg (ppr m)
        ) us

printExtPkgs env = do
    putMsgS "External Package Dependencies:"
    eps <- liftIO $ hscEPS env
    let pit = eps_PIT eps
    let elts = moduleEnvElts pit
    mapM_ (\e -> do 
                    (putMsg . ppr . mi_module) e
                    let sig = mi_complete_sigs e 
                    mapM_ (putMsg . ppr) sig
                    ) elts

printModules env = do
    let modGraph = hsc_mod_graph env
    let mods = mgModSummaries modGraph
    putMsgS "Module summaries:"
    mapM_ (putMsg . ppr) mods
    
printNameEnv env = do
    nameCache <- liftIO $ readIORef (hsc_NC env)
    let names = nsNames nameCache
    let elts = moduleEnvElts names
    putMsgS "Name Cache:"
    mapM_ (putMsg . ppr) elts 

getModuleNames env mod = do
    nameCache <- liftIO $ readIORef (hsc_NC env)
    let names = nsNames nameCache
    let menv  = lookupModuleEnv names mod
    return $ case menv of 
        Nothing -> []
        Just e  -> occEnvElts e

-- copied from https://github.com/nomeata/inspection-testing/blob/master/src/Test/Inspection/Plugin.hs
lookupNameInGuts :: ModGuts -> Name -> [(Var, CoreExpr)]
lookupNameInGuts guts n = 
            [ (v,e)
            | (v,e) <- flattenBinds (mg_binds guts)
            , getName v == n
            ]

debugPass :: ModGuts -> CoreM ModGuts
debugPass mod = 
      let printModuleName :: ModuleName -> CoreM ()
          printModuleName name = putMsgS ("\t" ++ moduleNameString name)
          printExport :: DynFlags -> AvailInfo -> CoreM ()
          printExport flags (Avail n) = putMsgS ("\t" ++ showSDoc flags (ppr n))
          printExport _ _ = putMsgS ""
          exports = mg_exports mod
      in do
        flags <- getDynFlags
        let deps = mg_deps mod
        putMsgS "dep_mods:"
        mapM_ printModuleName $ map fst (dep_mods deps)
        putMsgS "dep_pkgs:"
        mapM_ (\s -> putMsgS ("\t" ++ (unpackFS .installedUnitIdFS . fst) s)) (dep_pkgs deps)
        putMsgS "dep_plgins:"
        mapM_ printModuleName $ dep_plgins deps
        putMsgS "dep_orphs:"
        mapM_ (printModuleName . moduleName) $ dep_orphs deps
        putMsgS "dep_finsts:"
        mapM_ (printModuleName . moduleName) $ dep_finsts deps
        putMsgS "Exports: "
        mapM_ (printExport flags) exports
        return mod
