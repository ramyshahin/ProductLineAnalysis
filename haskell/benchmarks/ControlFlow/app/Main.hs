{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass, BangPatterns #-}
-- #define CASE_TERMINATION
-- #define RETURN 
#define RETURN_AVG

module Main where

import CFG
import qualified VCFG as V
import CFGParser
import SPL
import PresenceCondition 
import Debug.Trace
import Control.Exception
import Criterion.Main
import Criterion.Main.Options
import Options.Applicative
import GHC.Generics (Generic)
import Control.DeepSeq
import System.IO
import System.Environment

#ifdef CASE_TERMINATION
import CaseTermination
import qualified CaseTerminationDeep as Deep
analysis = "CaseTermination"
#endif

#ifdef RETURN
import Return
import qualified ReturnDeep as Deep
analysis = "Return"
#endif

#ifdef RETURN_AVG
import ReturnAvg
import qualified ReturnAvgDeep as Deep
analysis = "Return Average"
#endif

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

--bruteforce :: (Var CFG, [String]) -> Var [CFGNode]
bruteforce (ns, features) = 
    let configs  = getAllConfigs features
        inVecs'   = zip (map (index ns) configs) configs
        inVecs    = filter (\(i, _) -> not (null i)) inVecs'
        --(Var ns') = ns
    in  --trace (show (length ns')) $
        mkVars $ map (\(input, pc) -> --trace (show pc) $
                            (analyze (head input), pc)) 
                     inVecs

shallow c = (liftV analyze) c

deep c = Deep.analyze c

nodes' = liftV nodes

data Env = Env {
    deepCFG     :: Var V.CFG,
    shallowCFG  :: Var CFG,
    fileName    :: String,
    features    :: [String],
    configs     :: Int,
    nodeCount   :: Int,
    hdr         :: String
    } deriving (Generic, NFData)

setupEnv filename = do
    !cfg <- readCFG filename
    let !nodes = (V._nodes cfg)
    let !nodeCount = nodes `seq` length nodes
    !features <- cfg `seq` getFeatures
    let !deep = cfg ^| ttPC
    let !shallow@(Var sh') = V.toShallowCFG cfg
    let !featCount = deep `seq` shallow `seq` length features
    let !configCount = length (getAllConfigs features)
    let presentConfigs = length sh'
    let hdr = foldr (\s t -> s ++ "," ++ t) "" 
            [filename, show nodeCount, show featCount, show configCount, show presentConfigs]
    let env = Env deep shallow filename features configCount nodeCount hdr
    putStrLn $ "Analysis:        " ++ analysis
    putStrLn $ "File:            " ++ filename
    putStrLn $ "Node#:           " ++ (show $ nodeCount)
    --putStrLn $ "Features:        " ++ (show features)
    putStrLn $ "Feature#:        " ++ (show $ featCount)
    putStrLn $ "Config#:         " ++ (show $ configCount)
    putStrLn $ "Present config#: " ++ (show $ presentConfigs)
    return env

reportResults s cfg = do
    let result = s cfg
    putStrLn $ show result

data CustomArgs = CustomArgs {
    filename :: String,
    others   :: Mode
}

customParser :: Parser CustomArgs
customParser = CustomArgs
    <$> strOption (
            long "filename"
        <>  value "input file name"
        <>  metavar "STR"
        <>  help "Input CFG to process."
    )
    <*> parseWith defaultConfig

--{-
main = do  
    --putStrLn $ "Analysis: " ++ analysis
    --handle <- openFile "files.txt" ReadMode
    --contents <- hGetContents handle
    --let files = lines contents
    as <- execParser $ describeWith customParser
    --if (null as) then putStrLn "File name missing." else putStrLn $ "Processing: " ++ (filename as)
    let file = filename as
    --defaultMainWith c $
    runMode (others as) 
                        [ env (setupEnv file) $ \ env -> bgroup (hdr env)
                                    [   bench "brute-force" $ nf bruteforce (shallowCFG env, features env),
                                        bench "shallow"     $ nf shallow    (shallowCFG env),
                                        bench "deep"        $ nf deep       (deepCFG env)
                                    ]
                        ]
---}

{-
tBruteforce = do
    env <- setupEnv
    let result = bruteforce (shallowCFG env, features env)
    putStrLn (show result)
    return result

tShallow = do
    env <- setupEnv
    let result = shallow (shallowCFG env)
    putStrLn (show result)
    return result

tDeep = do
    env <- setupEnv
    let result = deep (deepCFG env)
    --putStrLn (show result)
    return result

main = defaultMain [ bgroup "main"
                        [   --bench "brute-force" $ nfIO tBruteforce, -- (cfg, feats),
                            bench "shallow"     $ nfIO tShallow,    --cfg,
                            bench "deep"        $ nfIO tDeep        --cfg
                            ] ]
-}
{-
fname = "head.cfg"

main = do
    env <- setupEnv fname
    --putStrLn $ "Features: " ++ (show feats)
    let result = deep $ deepCFG env
    --let result = bruteforce (shallowCFG env, features env) 
    putStrLn $ show result
    putStrLn "Done."
-}