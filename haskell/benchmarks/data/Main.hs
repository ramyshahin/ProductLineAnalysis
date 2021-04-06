module Main where

import Criterion.Main
import Criterion.Main.Options
import SPL
import ListDeep
import System.IO
import System.Environment

mkList :: Int -> List_ Int
mkList n = 
    if n == 0 
    then c_Nil
    else c_Cns (mkVarT n) (mkList (n-1))

test :: List_ Int -> Var Int
test l = len l

main = do
   args <- getArgs
   let n = read (head args) :: Int
   let xs = mkList n
   let result = test xs
   putStrLn $ show result 
{-
main = defaultMain [
    bgroup "DeepList" [bench "100" $ nf test 100
                      ,bench "1000" $ nf test 1000
                      ,bench "10000" $ nf test 10000
                      ,bench "100000" $ nf test 100000]
    ]
-}