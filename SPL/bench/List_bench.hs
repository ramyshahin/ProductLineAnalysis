import Criterion.Main
import System.Random
import Control.Monad(replicateM)

import SPL
import Prop
import Deep.VList as D
import Shallow.VList as S

featCount = 2
count = 5

genFeats :: [Int] -> [String]
genFeats [] = []
genFeats (i:is) = ("f" ++ (show i)) : genFeats is

u :: Universe
u = mkUniverse $ genFeats [1..featCount]

rndTerm :: IO Prop
rndTerm = do
    r <- (randomIO :: IO Int)
    let atm = Atom u (mod r featCount)
    s <- (randomIO :: IO Int)
    return (if ((mod s 2) == 0) then (neg atm) else atm)
    
rndPC :: IO Prop
rndPC = do
    c <- (randomIO :: IO Int)
    terms <- replicateM (mod c featCount) rndTerm
    return (conj terms)

shallowBench pairs = 
    let vs = map (\(x,pc) -> mkVar x pc) pairs
        xs = S.mkVList vs
    in  S.vmap (mkVarT (+ 1)) xs

deepBench pairs = 
    let vs = map (\(x,pc) -> mkVar x pc) pairs
        xs = D.mkVList vs
    in  D.vmap (mkVarT (+ 1)) xs

main :: IO ()
main = do
    xs_ <- replicateM count (randomIO :: IO Int) 
    let xs = map (\x -> mod x 100) xs_
    pcs <- replicateM count rndPC
    let pairs = zip xs pcs
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
    {-
    defaultMain [
        --bench "shallow" $ whnf shallowBench pairs --,
        bench "deep" $ whnf deepBench pairs
        ]-}