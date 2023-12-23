-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestAppDeep where
{-
import SPL
import Data.List

x :: V Int
x = v 7

xs :: V [Int]
xs = v [3,6,11]

foo :: V Int -> V Int
foo x  = (v (*)) <*> (v x) <*> (v 2) 

bar :: V Int -> V Int -> V Int 
bar x y  = (v (+)) <*> (v foo <*> x) <*> v y

baz :: V Int -> V Int -> V Int -> V Int 
baz a b c  = (v (-)) <*> (v bar <*> a <*> b) <*> (v bar <*> c <*> b)
-}