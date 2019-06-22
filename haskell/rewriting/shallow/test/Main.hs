module Main where

import TestDeep
import SPL
import PropBDD

p, q, r, s :: Prop
univ@[p, q, r, s] = mkUniverse ["P", "Q", "R", "S"]

pq = conj[p,q]
p_q = conj[p, neg q]
_pq = conj[neg p, q]
_p_q = conj[neg p, neg q]
_p = neg p
_q = neg q 

v1, v2 :: Var Int
v1 = mkVars [(1,pq), (2,p_q), (1, _pq), (2, _p_q)]
v2 = mkVars [(10,q), (0, _q)]

w :: Var Int
w = mkVars [(12, pq), (2, p_q), (3, _p_q)]

result = foo v1 v2 w 

result2 = c v1 v2 

main = do
    putStr $ show result
    putStr $ show result2