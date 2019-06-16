module GhcRewriter(plugin) where

import GhcPlugins
import HsSyn

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = coreToDos,
  parsedResultAction = modifyPreamble
  }
  
modifyPreamble :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
modifyPreamble _ _ m = (renameModule m) >>= addDependencies

coreToDos :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreToDos _ todo = do
  putMsgS "Hello!"
  return ((CoreDoPluginPass "Say name" pass) : todo)

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

addDependencies :: HsParsedModule -> Hsc HsParsedModule
addDependencies m = 
  let hsMod@(L s mod) = hpm_module m
      imports         = hsmodImports mod
      splImport       = simpleImportDecl (mkModuleName "SPL")
      imports'        = (L noSrcSpan splImport) : imports
      mod'            = m { hpm_module = L s (mod { hsmodImports = imports'}) }
  in  do
    return mod'

pass :: ModGuts -> CoreM ModGuts
pass mod = bindsOnlyPass (mapM printBind) mod
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bndr@(NonRec b _) = do
          flags <- getDynFlags
          putMsgS $ "Non-recursive binding name " ++ (showSDoc flags (ppr b))
          return bndr
        printBind bndr = return bndr