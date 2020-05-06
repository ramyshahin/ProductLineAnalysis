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

inputFileName = "/mnt/f/code/busybox-1.18.5/libbb/lineedit.cfg"

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

bruteforce (ns, features) = 
    let configs  = getAllConfigs features
        inVecs   = zip (map (configIndex ns) configs) configs
    in  --trace (show features) $
        mkVars $ map (\(input, pc) -> 
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
    cfg <- readCFG inputFileName
    let nodes = (V._nodes cfg)
    let nodeCount = nodes `seq` length nodes
    features <- cfg `seq` getFeatures
    let deep = cfg ^| ttPC
    let shallow = V.toShallowCFG cfg
    let featCount = deep `seq` shallow `seq` length features
    let configCount = length (getAllConfigs features)
    let env = Env deep shallow inputFileName features configCount nodeCount
    putStrLn $ "File:     " ++ inputFileName
    putStrLn $ "Features: " ++ (show features)
    putStrLn $ "Feature#: " ++ (show $ featCount)
    putStrLn $ "Config#:  " ++ (show $ configCount)
    putStrLn $ "Node#:    " ++ (show $ nodeCount)
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
test t = do
    cfg <- readCFG inputFileName
    features <- cfg `seq` getFeatures
    return $ t (cfg, features)

main = defaultMain [ bgroup "main"
                        [   bench "brute-force" $ nfIO (test bruteforce), -- (cfg, feats),
                            bench "shallow"     $ nfIO (test shallow),    --cfg,
                            bench "deep"        $ nfIO (test deep)       --cfg
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