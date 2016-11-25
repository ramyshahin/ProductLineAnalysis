-- Variability unit tests
import Variability
import Test.HUnit

x :: SPLVariable Int
x = V [(3, True), (-6, True), (-2, True)]

y :: SPLVariable Int
y = V [(-7, True), (2, True), (-1, True)]

xAbs = apply (liftT abs) x
yAbs = apply (liftT abs) y

x_plus_y = apply2 (liftT (+)) x y