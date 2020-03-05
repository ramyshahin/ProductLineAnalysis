-- TestVal.hs
-- Some simple tests for deep lifting of values

module TestVal where

-- simple values
x :: Int
x = 7

foo :: [a] -> a
foo = head
