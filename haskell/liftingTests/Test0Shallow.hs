{-# LANGUAGE LambdaCase #-}

module Test0Shallow where
import Test0
import SPL
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

-- testing conditionals
c a b = if (a > b) then bar a b else b - a

-- testing ADTs
data MaybeInt =
   None
 | Some Int
 
-- testing recursive Algebraic types
--data ListInt =
--    Nil
--  | Cons Int ListInt

--head :: ListInt -> Int
--head xs = case xs of
--     Nil -> 0
--     Cons x xs -> x
