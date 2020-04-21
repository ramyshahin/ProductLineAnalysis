module Main where

import qualified TokenCount as B
import qualified TokenCountDeep as D
import Criterion.Main
import VCPP
import Lexer
import PropBDD
import System.IO.Unsafe

filename = "/mnt/f/code/busybox-1.31.1/util-linux/chrt.c"

inputTokens = do
    file <- readFile (filename)
    case lexer "" file of
        Left  e -> print e >> fail "parse error"
        Right r ->  let result = vcpp mkCPPEnv r
                    in  do 
                            --(putStrLn (show result)) >> putStrLn ("Vars: " ++ (show getVars))
                            return result

tokenCount' ts = do
    let result = D.tokenCount ts
    putStrLn $ "Results: " ++ (show result)
    return result

main :: IO ()
--main = defaultMain [ bench "Deep" $ whnfIO (inputTokens >>= tokenCount') ]
main = do
    inputs <- inputTokens
    tokenCount' inputs
    return ()