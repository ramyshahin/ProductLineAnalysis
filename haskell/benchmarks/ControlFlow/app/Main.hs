module Main where

import CFGParser

inputFileName = "/mnt/f/code/busybox-1.18.5/coreutils/head.cfg.dot"

main :: IO ()
main = do
    input <- inputGraph inputFileName
    nodes <- debugGraph input 
    return ()