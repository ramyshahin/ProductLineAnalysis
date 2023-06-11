{-# LANGUAGE NoImplicitPrelude #-}module Test0Deep where
import SPL
import VPrelude
import Data.List

x :: Int
x = (3 ^| ttPC)

y :: Int
y = (7 ^| ttPC)

z :: Int
z = (9 ^| ttPC)

foo :: Int -> Int -> Int -> Int
foo x y z  = ((+) ^| ttPC) <*> ((bar x y)) <*> ((baz z))

bar :: Int -> Int -> Int
bar x y  = ((+) ^| ttPC) <*> (x) <*> (y)

baz :: Int -> Int
baz x  = neg' <*> (x)

f = baz
g = baz

x' = f (f (g (2 ^| ttPC)))

--xs :: [Int]
--xs = [3]

c a b  = liftedCond ((((>) ^| ttPC) <*> (a) <*> (b))) (\__cntxt__ -> bar a b) (\__cntxt__ -> ((-) ^| __cntxt__) <*> (b) <*> (a))

data MaybeInt = MaybeInt_ { fone :: None, fome :: Some }

MaybeInt = MaybeInt None Some
 
-- testing recursive Algebraic types
--data ListInt =
--    Nil
--  | Cons Int ListInt

--head :: ListInt -> Int
--head xs = case xs of
--     Nil -> 0
--     Cons x xs -> x
