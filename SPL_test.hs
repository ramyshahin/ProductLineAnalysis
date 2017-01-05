-- SPL unit tests
import SPL
import Test.HUnit

x :: Var Int
x = [(3, True), (-6, True), (-2, True)]

y :: Var Int
y = [(-7, True), (2, True), (-1, True)]

xAbs = apply (liftT abs) x
yAbs = apply (liftT abs) y

x_plus_y = apply2 (liftT (+)) x y


