module Main where

import CFG
import CFGParser
import CaseTermination
import qualified CaseTerminationDeep as Deep
import SPL
import PresenceCondition 
import Debug.Trace
import Control.Exception
import Criterion.Main

inputFileName = "/mnt/f/code/busybox-1.18.5/miscutils/less.cfg"

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

bruteforce (ns, features) = 
    let configs  = getAllConfigs features
        inVecs   = zip (map (configIndex ns) configs) configs
    in  mkVars $ map (\(input, pc) -> 
                            (analyze input, pc)) 
                     inVecs

shallow = liftV analyze

deep = Deep.analyze

nodes' = liftV nodes

setupEnv = do
    cfg <- readCFG inputFileName
    features <- cfg `seq` getFeatures
    putStrLn $ "File:     " ++ inputFileName
    putStrLn $ "Features: " ++ (show features)
    putStrLn $ "Feature#: " ++ (show $ length features)
    putStrLn $ "Config#:  " ++ (show $ length (getAllConfigs features))
    return (cfg, features)

reportResults s cfg = do
    let result = s cfg
    putStrLn $ show result

{-
main = defaultMain [ env setupEnv $ \ ~(cfg, feats) -> bgroup "main"
                        [   bench "brute-force" $ nf bruteforce (cfg, feats),
                            bench "shallow"     $ nf shallow    cfg,
                            bench "deep"        $ nf deep       cfg
                            ] ]
-}

--{-
main = do
    (cfg, _) <- setupEnv
    let result = deep cfg
    putStrLn $ show result
    putStrLn "Done."
-- -}