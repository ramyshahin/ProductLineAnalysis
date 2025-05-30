--{-# LANGUAGE NoImplicitPrelude #-}
module TestPrelude where

import SPL
import PresenceCondition
import VPrelude
import Test.QuickCheck

p, q, r, s :: PresenceCondition
[p, q, r, s] = map mkFeature ["P", "Q", "R", "S"]

_p = negPC p
_q = negPC q
pq = p /\ q
p_q = p /\ _q
_pq = _p /\ q
_p_q = _p /\ _q

v1, v2 :: VInt
v1 = mkVars [(1,pq), (2,p_q), (1, _pq), (2, _p_q)]
v2 = mkVars [(10,q), (0, _q)]

w :: VInt
w = mkVars [(12, pq), (2, p_q), (3, _p_q)]

foo :: VInt -> VInt -> VInt -> VInt
foo x y z = x + y + z 

result = foo v1 v2 w 

c :: VInt -> VInt -> VInt
c x y = x - y

result2 = c v1 v2 
