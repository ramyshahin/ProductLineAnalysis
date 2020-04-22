module Main where

import ControlFlow

main :: IO ()
main = do
    input <- inputGraph
    debugGraph input 
