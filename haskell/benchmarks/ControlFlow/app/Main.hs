module Main where

import CFG
import CFGParser
import CaseTermination
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

analyze' = liftV analyze

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes = filter (\n -> case n of 
                                    (CFGNode _ _ (CFGFunc _) _ _)   -> True
                                    _                               -> False)
getFunctionNodes' = liftV getFunctionNodes

runBruteForce :: Var CFG -> Var [CFGNode]
runBruteForce ns = 
    let features = getFeatures ns
        configs  = getAllConfigs features
        inVecs   = zip (map (configIndex ns) configs) configs
    in  mkVars $ map (\(input, pc) -> 
                            --trace (show pc) 
                            (analyze input, pc)) 
                     inVecs

runShadowLifted :: Var CFG -> Var [CFGNode]
runShadowLifted ns = analyze' ns

main :: IO ()
main = do
    cfg <- readGraph inputFileName
    let features = getFeatures cfg
    let result = runBruteForce cfg
    putStrLn $ "Features: " ++ (show features)
    putStrLn $ show result
    return ()