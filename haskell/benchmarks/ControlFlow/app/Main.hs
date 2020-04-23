module Main where

import ControlFlow

main :: IO ()
main = do
    input <- inputGraph
    nodes <- debugGraph input 
    return ()