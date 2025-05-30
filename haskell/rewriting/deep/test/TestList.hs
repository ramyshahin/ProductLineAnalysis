module TestList where
import VPrelude
import SPL
import PresenceCondition
import Test.QuickCheck

p, q, r, s :: PresenceCondition
[p, q, r, s] = map mkFeature ["P", "Q", "R", "S"]

_p = negPC p
_q = negPC q
pq = p /\ q
p_q = p /\ _q
_pq = _p /\ q
_p_q = _p /\ _q

--emptyList :: VList Int
--emptyList = Nil


