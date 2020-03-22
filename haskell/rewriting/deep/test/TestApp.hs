-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestApp where
import Data.List

-- simple values
x :: Int
x = 7

xs :: [Int]
xs = [3,6,11]

foo :: Int -> Int
foo x = x * 2 

bar :: Int -> Int -> Int 
bar x y = (foo x) + y

baz :: Int -> Int -> Int -> Int 
baz a b c = (bar a b) - (bar c b)