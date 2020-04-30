{-# LANGUAGE CPP #-}
-- #define DEEP
#define BRUTE_FORCE

module Main where

import CFG
import CFGParser
import CaseTermination
import qualified CaseTerminationDeep as Deep
import SPL
import PresenceCondition 
import Debug.Trace
import Control.Exception

inputFileName = "/mnt/f/code/busybox-1.18.5/coreutils/head.cfg.dot"

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

run :: Var CFG -> Var [CFGNode]

#ifdef SHALLOW
run = liftV analyze
#endif

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

#ifdef BRUTE_FORCE
run ns = 
    let features = getFeatures ns
        configs  = getAllConfigs features
        inVecs   = zip (map (configIndex ns) configs) configs
    in  mkVars $ map (\(input, pc) -> 
                            (analyze input, pc)) 
                     inVecs
#endif

#ifdef DEEP
run = Deep.analyze
#endif

nodes' = liftV nodes

main :: IO ()
main = do
    cfg <- readGraph inputFileName
    let features = trace ("Main: node counts: " ++ (show (length' (nodes' cfg)))) $ getFeatures cfg
    let result = run cfg
    putStrLn $ "Features: " ++ (show features)
    putStrLn $ show result
    return ()