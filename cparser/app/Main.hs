-- Main.hs
-- Lifted C Parser driver

module Main where

import CParser
import VCPP
import SPL
import System.Environment ( getArgs, getProgName)

main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args!!0)
    let pp = vcpp file
    let pos = startPosition (args!!0)
    let parsed = parseC' (inputStreamFromString' pp) (mkVarT pos) 
    putStrLn (show parsed)

    {-mapM_ (\(x,pc) -> case x of
                        Just x' -> putStrLn ((show pc) ++ ":\n" ++ (show x') ++ "\n")
                        Nothing -> putStrLn ((show pc) ++ ": Nothing\n")) parsed
-}