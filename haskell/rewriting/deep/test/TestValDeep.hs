-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestValDeep where
import SPL
import Data.List

x :: Var Int
x = (mkVarT 7)

xs :: Var [Int]
xs = mkVarT [3,6,11]

foo :: Var [a] -> Var a
foo = mkVarT head

foo' :: Var [a] -> Var a
foo' = mkVarT foo

