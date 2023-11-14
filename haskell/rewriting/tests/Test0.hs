
module Test0 
where

x :: Int
x = 3

y :: Int
y = 7

z :: (Int)
z = 9

u :: ()
u = ()

foo :: Int -> Int -> Int -> Int
foo x y z = (bar x y) + (baz z)

bar :: Int -> Int -> Int
bar x y = x + y

baz :: Int -> Int
baz x = negate x

f = baz
g = baz

x' = f (f (g 2))

xs :: [Int]
xs = [3]
-- testing conditionals
c a b = if (a > b) then bar a b else b - a