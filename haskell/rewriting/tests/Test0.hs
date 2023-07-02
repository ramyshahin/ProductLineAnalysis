
module Test0 
where

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
baz x = negate x

f = baz
g = baz

x' = f (f (g 2))

--xs :: [Int]
--xs = [3]

-- testing conditionals
c a b = if (a > b) then bar a b else b - a

--
-- Testing simple (0-ary constructor) sum types
--
data Direction =
   North
 | South
 | East
 | West 

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