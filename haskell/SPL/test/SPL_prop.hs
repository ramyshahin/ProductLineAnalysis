-- SPL quickcheck properties
{-# LANGUAGE TemplateHaskell #-}
module SPL_prop where

import Test.QuickCheck.All
import SPL
import PropBDD
import PresenceCondition
--import Shallow.VList as S
--import Deep.VList as D
import Control.Applicative

import Debug.Trace

p, q, r, s :: Prop
[p, q, r, s] = map mkFeature ["P", "Q", "R", "S"]

pq = p /\ q
p_q = p /\ (negPC q)
_pq = (negPC p) /\ q
_p_q = (negPC p) /\ (negPC q)
_p = negPC p
_q = negPC q 

v1, v2 :: V Int
part1 = mkPartition [pq, p_q, _pq, _p_q]
part2 = mkPartition [q, _q]
part3 = mkPartition [pq, p_q, _p_q]
v1 = annotate part1 [1, 2, 1, 2]
v2 = annotate part2 [1, 2]

w :: V Int
w = annotate part3 [12, 2, 3]

x :: V Int
x = annotate part1 [7, -3, -8, 0]

xs = [1..5]

-- ===
prop_phys_eq1 = 1 === 1
prop_phys_eq2 = not (1 === 2)
prop_phys_eq3 = x === x
prop_phys_eq4 = not (w === x)
prop_phys_eq5 = (head xs === head xs)
prop_phys_eq6 = (tail xs === tail xs)

{-
-- exists
prop_exists1 = exists (2, p_q) w 
prop_exists2 = exists ((-11), _pq) y
prop_exists3 = not (exists (4,p_q) w)
prop_exists4 = not (exists ((-8), q) x)

-- compact
prop_compact1 = (compact x == x)
prop_compact2 = (compact z == z)
prop_compact3 = (compact v1 == v2)

-- index
prop_index1 = index x p == [7,-3]
prop_index2 = index x pq == [7]
prop_index3 = index z _p == []

-- getAllConfigs
--prop_getAllConfigs1 = length (getAllConfigs univ) == 16

-- getValidConfigs
--prop_getValidConfigs1 = length (getValidConfigs univ p) == 8
--prop_getValidConfigs2 = length (getValidConfigs univ pq) == 4

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

list0 = mkVar ttPC []
list1 = x ^: list0

--null' = liftV null
--tail' = liftV tail

length' :: Var [a] -> Var Int
length' xs = liftedCond (null' xs) 
                        (\__cntxt__ -> (mkVar 0 __cntxt__))
                        (\__cntxt__ -> (mkVar 1 __cntxt__) ^+ (length' (tail' (restrict __cntxt__ xs))))

prop_list0 = (length' ttPC list0) == (0 ^| ttPC)
prop_list1 = (length' ttPC list1) == (1 ^| ttPC)

xs' = mkVars [([1,2,3,4], p), ([3,2], _p)]
prop_list2 = (length' ttPC xs') == mkVars [(4, p), (2, _p)]

-- unary function: abs
abs' = pure abs

xAbs = abs' <*> x
yAbs = abs' <*> y

prop_xabs = xAbs == mkVars [(7, pq), (3, p_q), (8, _pq), (0, _p_q)]
prop_yabs = yAbs == mkVars [(11, _p)]

-- binary function (|+|)
x_plus_y0 = apply (apply (pure (+)) x) y
x_plus_y1 = (pure (+)) <*> x <*> y
x_plus_y2 = x ^+ y

prop_plus0 = x_plus_y0 == mkVars [(-19,_pq), (-11,_p_q)]
prop_plus1 = x_plus_y0 == x_plus_y1
prop_plus2 = x_plus_y0 == x_plus_y2

-- safe division and lifted conditionals
safeDiv :: Int -> Int -> Int
safeDiv a b = if b == 0 then 0 else (div a b)

div' c = div ^| c

zero c = (0 ^| c)
one = (1 ^|)

safeDiv' :: Var Int -> Var Int -> Var Int
safeDiv' a b = liftedCond (b ^== zero) (\__cntxt__ -> zero /^ __cntxt__) (\__cntxt__ -> div' (a /^ __cntxt__) (b /^ __cntxt__))
divResult = safeDiv' w x

prop_safeDiv = divResult == mkVars [(1,pq), (-1, p_q), (0,_p_q)]
-}

return []
runSPLTests = $quickCheckAll
