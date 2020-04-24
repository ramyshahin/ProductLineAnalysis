module Main where

import CFGParser

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

main :: IO ()
main = do
    nodes <- readGraph inputFileName 
    putStrLn $ show nodes 
    return ()