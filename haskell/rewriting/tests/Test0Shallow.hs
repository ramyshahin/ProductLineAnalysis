{-# LANGUAGE LambdaCase #-}

module Test0Shallow where
import qualified Test0 as O
import SPL
import Data.List

x :: Var Int
x = mkVarT O.x

y :: Var Int
y = mkVarT O.y

z :: Var Int
z = mkVarT O.z

foo :: Var Int -> Var Int -> Var Int -> Var Int
foo x y z  = (mkVarT O.foo) <*> x <*> y <*> z

bar :: Var Int -> Var Int -> Var Int
bar x y  = (mkVarT O.bar) <*> x <*> y

baz :: Var Int -> Var Int
baz x  = (mkVarT O.baz) <*> x

f = mkVarT O.f
g = mkVarT O.g

x' = mkVarT O.x'

xs :: Var [Int]
xs = mkVarT O.xs

c a b  = (mkVarT O.c) <*> a <*> b

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
