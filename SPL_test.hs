-- SPL unit tests
import SPL
import Prop
import Test.HUnit
import Control.Applicative

x :: Var Int
x = Var [(3, T), (-6, T), (-2, T)]

y :: Var Int
y = Var [(-7, T), (2, T), (-1, T)]

xAbs = apply (liftT abs) x
yAbs = apply (liftT abs) y

x_plus_y0 = apply2 (liftT (+)) x y
x_plus_y1 = (pure (+)) <*> x <*> y
x_plus_y2 = (liftA2 (+)) x y

