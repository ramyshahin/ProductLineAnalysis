import Criterion.Main
import System.Random
import Control.Monad(replicateM)

import SPL
import Prop
import Deep.VList as D
import Shallow.VList as S

u :: Universe
u = mkUniverse ["P", "Q", "R", "S"]

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

count = 1000

shallowBench pairs = 
    let vs = map (\(x,pc) -> mkVar x pc) pairs
        xs = S.mkVList vs
    in  S.vmap (mkVarT (* 2)) xs

deepBench pairs = 
    let vs = map (\(x,pc) -> mkVar x pc) pairs
        xs = D.mkVList vs
    in  D.vmap (mkVarT (* 2)) xs

main :: IO ()
main = do
    xs <- replicateM count (randomIO :: IO Int) 
    let pcCount = length pcsAll
    let randPC = do 
            index <- randomRIO (0, pcCount-1)
            return (pcsAll !! index)
    pcs <- replicateM count randPC
    let pairs = zip xs pcs
    defaultMain [
        bench "deep" $ whnf deepBench pairs,
        bench "shallow" $ whnf shallowBench pairs
        ]

