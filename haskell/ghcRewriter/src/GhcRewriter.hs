module GhcRewriter(plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }
  
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Hello!"
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass mod = bindsOnlyPass (mapM printBind) mod
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bndr@(NonRec b _) = do
          flags <- getDynFlags
          putMsgS $ "Non-recursive binding name " ++ (showSDoc flags (ppr b))
          return bndr
        printBind bndr = return bndr