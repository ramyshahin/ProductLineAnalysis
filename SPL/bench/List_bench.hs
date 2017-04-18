import Criterion.Main
import System.Random
import Control.Monad(replicateM)

import SPL
import Prop
import Deep.VList as D
import Shallow.VList as S

featCount = 2

genFeats :: [Int] -> [String]
genFeats [] = []
genFeats (i:is) = ("f" ++ (show i)) : genFeats is

u :: Universe
u = mkUniverse $ genFeats [1..featCount]

p, q, r, s :: Prop
p = Atom u 0
q = Atom u 1
r = Atom u 2
s = Atom u 3

pq = conj[p,q]
p_q = conj[p, neg q]
_pq = conj[neg p, q]
_p_q = conj[neg p, neg q]
_p = neg p
_q = neg q 

pcsAll = [T, F, p, q, _p, _q, pq, p_q, _pq, _p_q]

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

count = 8

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
    let pcCount = length pcsAll
    pcs <- replicateM count rndPC
    let pairs = zip xs pcs
    putStrLn $ show xs
    putStrLn $ show pcs
    putStrLn $ show (shallowBench pairs)
    putStrLn $ show (deepBench pairs)
    {-
    defaultMain [
        --bench "shallow" $ whnf shallowBench pairs --,
        bench "deep" $ whnf deepBench pairs
        ]
-}