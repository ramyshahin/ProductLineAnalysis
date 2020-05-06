{-# LANGUAGE DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module Main where

import CFG
import qualified VCFG as V
import CFGParser
import CaseTermination
import qualified CaseTerminationDeep as Deep
import SPL
import PresenceCondition 
import Debug.Trace
import Control.Exception
import Criterion.Main
import GHC.Generics (Generic)
import Control.DeepSeq

inputFileName = "/mnt/f/code/busybox-1.18.5/miscutils/less.cfg"

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

bruteforce :: (Var CFG, [String]) -> Var [CFGNode]
bruteforce (ns, features) = 
    let configs  = getAllConfigs features
        inVecs   = zip (map (configIndex ns) configs) configs
        (Var ns') = ns
    in  --trace (show (length ns')) $
        mkVars $ map (\(input, pc) -> --trace (show pc) $
                            (analyze input, pc)) 
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
    nodeCount   :: Int
    } deriving (Generic, NFData)

setupEnv = do
    !cfg <- readCFG inputFileName
    let !nodes = (V._nodes cfg)
    let !nodeCount = nodes `seq` length nodes
    !features <- cfg `seq` getFeatures
    let !deep = cfg ^| ttPC
    let !shallow@(Var sh') = V.toShallowCFG cfg
    let !featCount = deep `seq` shallow `seq` length features
    let !configCount = length (getAllConfigs features)
    let env = Env deep shallow inputFileName features configCount nodeCount
    putStrLn $ "File:            " ++ inputFileName
    putStrLn $ "Node#:           " ++ (show $ nodeCount)
    --putStrLn $ "Features:        " ++ (show features)
    putStrLn $ "Feature#:        " ++ (show $ featCount)
    putStrLn $ "Config#:         " ++ (show $ configCount)
    putStrLn $ "Present config#: " ++ (show $ (length sh'))
    return env

reportResults s cfg = do
    let result = s cfg
    putStrLn $ show result

--{-
main = defaultMain [ env setupEnv $ \ ~env -> bgroup "main"
                        [   bench "brute-force" $ nf bruteforce (shallowCFG env, features env),
                            bench "shallow"     $ nf shallow    (shallowCFG env),
                            bench "deep"        $ nf deep       (deepCFG env)
                            ] ]
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
main = do
    (cfg, feats) <- setupEnv
    putStrLn $ "Features: " ++ (show feats)
    let result = deep (cfg ^| ttPC, feats)
    putStrLn $ show result
    putStrLn "Done."
-}