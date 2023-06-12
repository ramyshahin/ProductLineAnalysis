{-# LANGUAGE NoImplicitPrelude #-}module Test0Deep where
import SPL
import VPrelude
import Data.List

x :: VInt
x = (3 ^| ttPC)

y :: VInt
y = (7 ^| ttPC)

z :: VInt
z = (9 ^| ttPC)

foo :: VInt -> VInt -> VInt -> VInt
foo x y z  = (bar x y) + (baz z)

bar :: VInt -> VInt -> VInt
bar x y  = x + y

baz :: VInt -> VInt
baz x  = negate x

f = baz
g = baz

x' = f (f (g (2 ^| ttPC)))

--xs :: [Int]
--xs = [3]

c a b  = liftedCond ((a > b)) (\__cntxt__ -> bar a b) (\__cntxt__ -> b - a)

data None = None

data Some = Some VInt

data VMaybeInt = MaybeInt_ { fone :: None, fome :: Some }
 
-- testing recursive Algebraic types
--data ListInt =
--    Nil
--  | Cons Int ListInt

--head :: ListInt -> Int
--head xs = case xs of
--     Nil -> 0
--     Cons x xs -> x
