-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestAppDeep where
import SPL
import Data.List

x :: Var Int
x = (mkVarT 7)

xs :: Var [Int]
xs = mkVarT [3,6,11]

foo :: Var Int -> Var Int
foo x  = (mkVarT (*)) <*> mkVarT x <*> (mkVarT 2) 

bar :: Var Int -> Var Int -> Var Int 
bar x y  = (mkVarT (+)) <*> (mkVarT foo <*> x) <*> mkVarT y

baz :: Var Int -> Var Int -> Var Int -> Var Int 
baz a b c  = (mkVarT (-)) <*> (mkVarT bar <*> a <*> b) <*> (mkVarT bar <*> c <*> b)
