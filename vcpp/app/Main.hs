-- main.hs
-- command-line VCPP wrapper
-- Ramy Shahin
-- Feb. 19th 2017
module Main where
import VCPP
import SPL
import System.Environment ( getArgs, getProgName)

main :: IO ()
main = do 
    args <- getArgs
    file <- readFile (args!!0)
    let (Var v) = vcpp file
    mapM_ (\(x,pc) -> case x of
                        Just x' -> putStrLn ("#" ++ (show pc) ++ "#\n" ++ x' ++ "\n")
                        Nothing -> putStrLn ((show pc) ++ ": Nothing\n")) v