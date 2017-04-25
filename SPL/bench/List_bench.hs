import Criterion.Main
import System.Random
import Control.Monad(replicateM)

import SPL
import Prop
import Deep.VList as D
import Shallow.VList as S

featCount = 4
count = 1000

genFeats :: [Int] -> [String]
genFeats [] = []
genFeats (i:is) = ("f" ++ (show i)) : genFeats is

u :: [Prop]
u = mkUniverse $ genFeats [1..featCount]

rndTerm :: IO Prop
rndTerm = do
    r <- (randomIO :: IO Int)
    let atm = u !! (mod r featCount)
    s <- (randomIO :: IO Int)
    return (if ((mod s 2) == 0) then (neg atm) else atm)
    
rndPC :: IO Prop
rndPC = do
    c <- (randomIO :: IO Int)
    terms <- replicateM (mod c featCount) rndTerm
    return (conj terms)

shallowBench n pairs' = 
    let pairs = take n pairs'
        vs = map (\(x,pc) -> mkVar x pc) pairs
        xs = S.mkVList vs
    in  S.vmap (mkVarT (+ 1)) xs

main :: IO ()
main = do
    xs_ <- replicateM count (randomIO :: IO Int) 
    let xs = map (\x -> mod x 100) xs_
    pcs <- replicateM count rndPC
    let pairs = zip xs pcs
    {-
    putStrLn $ show xs
    putStrLn $ show pcs
    let res = shallowBench pairs
    let h = S.vhead res
    let t = S.vtail res
    print $ 1 === 2
    print $ res === res
    print $ compact h
    print $ compact t
    putStrLn $ show res
    putStrLn $ show h
    putStrLn $ show t

    --putStrLn $ show (deepBench pairs)
-}

    defaultMain [
        bench (show n) $ whnf (shallowBench n) pairs | n <- [500]
        ]