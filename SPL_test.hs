-- SPL unit tests
import SPL
import Prop
import Test.HUnit
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
_p = neg p

x :: Var Int
x = mkVars [(3, pq), (-6, p_q), (-2, _p)]

y :: Var Int
y = mkVars [(-7, pq), (2, p_q), (-1, _p)]

xAbs = apply (liftT abs) x
yAbs = apply (liftT abs) y

x_plus_y0 = apply (apply (liftT (+)) x) y
x_plus_y1 = (pure (+)) <*> x <*> y
x_plus_y2 = (liftA2 (+)) x y

a :: Var Int
a = mkVarT 0

l :: Var [Int]
l = mkVarT []

--m :: Var [String]
--m = mkVar [("", T)]