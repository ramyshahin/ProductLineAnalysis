import Criterion.Main
import System.Random
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
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

count = 100

shallowBench xs pcs = 
    let pairs = zip xs pcs
        vs = map (\(x,pc) -> mkVar x pc) pairs
    in  S.mkVList vs

main :: IO ()
--main = defaultMain [bench "len0" $ nf len0 [0..1000000] ]
main = do
    xs <- replicateM count (randomIO :: IO Int) 
    let pcCount = length pcsAll
    --pcs <- replicateM count (pcs !! (randomRIO (0, pcCount-1))) 
    pcs <- replicateM count (runRVar (choice pcsAll) DevRandom)
    defaultMain [bench "shallow" $ whnf (shallowBench xs) pcs]

