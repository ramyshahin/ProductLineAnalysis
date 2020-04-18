-- main.hs
-- command-line VCPP wrapper
-- Ramy Shahin
-- Feb. 19th 2017
module Main where
import Lexer
import VCPP
import SPL
import PropBDD
import System.Environment ( getArgs, getProgName)


main :: IO ()
main = do 
    args <- getArgs
    file <- readFile (args!!0)
    case lexer "" file of
        Left  e -> print e >> fail "parse error"
        Right r ->  let result = vcpp mkCPPEnv r
                    in  (putStrLn (show result)) >>
                         putStrLn ("Vars: " ++ (show getVars))
    --Var v <- vcpp file
    --mapM_ (\(x,pc) -> putStrLn ("#" ++ (show pc) ++ "#\n" ++ x ++ "#---#")) v