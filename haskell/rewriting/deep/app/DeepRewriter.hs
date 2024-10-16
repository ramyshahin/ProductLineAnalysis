module Main where

import Rewrite.Deep
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if (length args /= 1) 
    then putStrLn $ "Usage: deep-rewrite-exe module-name"
    else 
        --catch 
        (run (head args) "1:1-100:1")
        --(\_ -> errorWithStackTrace "error")