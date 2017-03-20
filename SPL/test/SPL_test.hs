-- SPL unit tests
import SPL
import Prop
--import Test.HUnit
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

w :: Var Int
w = mkVars [(12, pq), (2, p_q), (3, _p_q)]

x :: Var Int
x = mkVars [(7, pq), (-3, p_q), (-8, _pq), (0, _p_q)]

y :: Var Int
y = mkVars [(-11, _p)]

z :: Var Int
z = mkVars [(6, p)]

abs' = pure abs

xAbs = abs' <*> x
yAbs = abs' <*> y

--(|+|) = liftV2 (+)

x_plus_y0 = apply (apply (pure (+)) x) y
x_plus_y1 = (pure (+)) <*> x <*> y
x_plus_y2 = x |+| y

safeDiv :: Int -> Int -> Int
safeDiv a b = if b == 0 then 0 else (div a b)
div' = liftV2 div

zero = (mkVarT 0)
one = (mkVarT 1)
 
safeDiv' :: Var Int -> Var Int -> Var Int
safeDiv' a b = cond' (b |==| zero) zero (div' a b)

a :: Var Int
a = mkVarT 0

l :: Var [Int]
l = mkVarT []

xs = mkVarList [w,x,y,z]
xs' = mkVarList' (mkVarT [w, x, y, z])

length' :: Var [a] -> Var Int
length' = liftV length
