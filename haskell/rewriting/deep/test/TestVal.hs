-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestVal where
import Data.List

-- simple values
x :: Int
x = 7

xs :: [Int]
xs = [3,6,11]

foo :: [a] -> a
foo = head

foo' :: [a] -> a
foo' = foo
