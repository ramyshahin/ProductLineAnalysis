-- SPL quickcheck properties
{-# LANGUAGE TemplateHaskell #-}
module SPL_prop where

import Test.QuickCheck.All
import SPL
import Prop
import Shallow.VList as S
import Deep.VList as D
import Control.Applicative

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

v1, v2 :: Var Int
v1 = mkVars [(1,pq), (2,p_q), (1, _pq), (2, _p_q)]
v2 = mkVars [(1,q), (2, _q)]

w :: Var Int
w = mkVars [(12, pq), (2, p_q), (3, _p_q)]

x :: Var Int
x = mkVars [(7, pq), (-3, p_q), (-8, _pq), (0, _p_q)]
x0 = mkVars [(-8, _pq), (0, _p_q), (-3, p_q), (7, pq)]

y :: Var Int
y = mkVars [(-11, _p)]
y0 = mkVars [(-11, _pq)]

z :: Var Int
z = mkVars [(6, p)]

xs = [1..5]

-- ===
prop_phys_eq1 = 1 === 1
prop_phys_eq2 = not (1 === 2)
prop_phys_eq3 = y === y
prop_phys_eq4 = not (y === z)
prop_phys_eq5 = (head xs === head xs)
prop_phys_eq6 = (tail xs === tail xs)

-- compact
prop_compact1 = (compact x == x)
prop_compact2 = (compact z == z)
prop_compact3 = (compact v1 == v2)

-- exists
prop_exists1 = exists (2, p_q) w 
prop_exists2 = exists ((-11), _pq) y
prop_exists3 = not (exists (4,p_q) w)
prop_exists4 = not (exists ((-8), q) x)

-- Ord
prop_lt1 = y0 < y
prop_lt2 = not (y < y0)
prop_lt3 = not (x < x0)

prop_lte1 = y0 <= y
prop_lte2 = y0 <= y0
prop_lte3 = not (y <= y0)

-- Eq
prop_eq1 = x0 == x
prop_eq2 = x /= y
prop_eq3 = y0 /= y

-- Shallow VList
sl1 = S.mkVList [w, x, y, z]
sl2 = S.mkVList [z, y, x, w]

-- Deep VList
dl1 = D.mkVList [w, x, y, z]
dl2 = D.mkVList [z, y, x, w]

-- vhead
prop_vhead0 = (S.vhead) sl1 == mkVars [(12,pq), (2,p_q), (3,_p_q), (-8, _pq)]
prop_vhead1 = (S.vhead) sl2 == mkVars [(6,p), (-11, _p)]

-- vlength
prop_vlength0 = S.vlength sl1 == mkVars[(3,neg _pq), (2,_pq)]
prop_vlength1 = S.vlength sl2 == S.vlength sl1

-- unary function: abs
abs' = pure abs

xAbs = abs' <*> x
yAbs = abs' <*> y

prop_xabs = xAbs == mkVars [(7, pq), (3, p_q), (8, _pq), (0, _p_q)]
prop_yabs = yAbs == mkVars [(11, _p)]

-- binary function (|+|)
x_plus_y0 = apply (apply (pure (+)) x) y
x_plus_y1 = (pure (+)) <*> x <*> y
x_plus_y2 = x |+| y

prop_plus0 = x_plus_y0 == mkVars [(-19,_pq), (-11,_p_q)]
prop_plus1 = x_plus_y0 == x_plus_y1
prop_plus2 = x_plus_y0 == x_plus_y2

-- safe division and lifted conditionals
safeDiv :: Int -> Int -> Int
safeDiv a b = if b == 0 then 0 else (div a b)
div' = liftV2 div

zero = (mkVarT 0)
one = (mkVarT 1)
 
safeDiv' :: Var Int -> Var Int -> Var Int
safeDiv' a b = cond' (b |==| zero) zero (div' a b)
divResult = safeDiv' w x

prop_safeDiv = divResult == mkVars [(1,pq), (-1, p_q), (0,_p_q)]

return []
runSPLTests = $quickCheckAll
