module Test where
import Data.List

x :: Int
x = 3

y :: Int
y = 7

z :: Int
z = 9

foo :: Int -> Int -> Int -> Int
foo x y z = (bar x y) + (baz z)

bar :: Int -> Int -> Int
bar x y = x + y

baz :: Int -> Int
baz x = -(x)

f = baz
g = baz

x' = f (f (g 2))

xs :: [Int]
xs = [3]

type List a = [a]
y'' :: List Int
y'' = [5]