module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if (length args /= 1) 
    then putStrLn $ "Usage: Shallow module-name"
    else run (head args) "1:1-100:1"