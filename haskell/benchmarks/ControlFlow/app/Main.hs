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
import Control.DeepSeq

inputFileName = "/mnt/f/code/busybox-1.18.5/coreutils/head.cfg"

instance NFData (Var a)
  where 
    rnf _ = () --ExitSuccess = ()
    --rnf (ExitFailure _) = ()

{-
getCntxtContents :: Text2Node -> Text2Cntxt -> Context T.Text -> IO VNode
getCntxtContents txt2node txt2cntxt c = do
    (n@(CFGNode i t nt ps ss), pc) <- getCntxtContents txt2node txt2cntxt c
    putStrLn $ "Node: " ++ show t
    putStrLn $ "\tID: " ++ show i
    putStrLn $ "\tin-edges: " ++ show ps
    putStrLn $ "\tout-edges: " ++ show ss
    putStrLn $ "\tPC: " ++ show pc
    return (n, pc)
-}

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

bruteforce ns = 
    let features = getFeatures ns
        configs  = getAllConfigs features
        inVecs   = zip (map (configIndex ns) configs) configs
    in  mkVars $ map (\(input, pc) -> 
                            (analyze input, pc)) 
                     inVecs

shallow = liftV analyze

deep = Deep.analyze

nodes' = liftV nodes

setupEnv = do
    cfg <- readCFG inputFileName
    let features = --trace ("Main: node counts: " ++ (show (length' (nodes' cfg)))) $ 
            getFeatures cfg
    putStrLn $ "Features: " ++ (show features)
    return cfg

{-
main = defaultMain [ env setupEnv $ \cfg -> bgroup "main"
                        [   bench "brute-force" $ nf bruteforce cfg,
                            bench "shallow"     $ nf shallow    cfg,
                            bench "deep"        $ nf deep       cfg
                            ] ]
-}

-- {-
main = do
    cfg <- setupEnv
    let result = deep cfg
    putStrLn $ show result
    putStrLn "Done."
-- -}