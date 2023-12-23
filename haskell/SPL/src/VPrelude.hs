{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module VPrelude where

import qualified Prelude as P
import SPL 

--
-- Booleans
--
type VBool = V P.Bool 

(&&) :: VBool -> VBool -> VBool
a && b = (P.pure (P.&&)) P.<*> a P.<*> b 

(||) :: VBool -> VBool -> VBool
a || b = (P.pure (P.||)) P.<*> a P.<*> b 

not :: VBool -> VBool
not a = (P.pure P.not) P.<*> a 

class P.Eq a => VEq a where
    (==) :: V a -> V a -> VBool
    a == b = (P.pure (P.==)) P.<*> a P.<*> b 

    (/=) :: V a -> V a -> VBool
    a /= b = (P.pure (P./=)) P.<*> a P.<*> b 

type VOrdering = V P.Ordering 

class P.Ord a => VOrd a where 
    compare :: V a -> V a -> VOrdering
    compare x y = (P.pure P.compare) P.<*> x P.<*> y

    (<) :: V a -> V a -> VBool
    a < b = (P.pure (P.<)) P.<*> a P.<*> b

    (<=) :: V a -> V a -> VBool
    a <= b = (P.pure (P.<=)) P.<*> a P.<*> b

    (>)  :: V a -> V a -> VBool
    a > b = (P.pure (P.>)) P.<*> a P.<*> b

    (>=) :: V a -> V a -> VBool
    a >= b = (P.pure (P.>=)) P.<*> a P.<*> b 

    max :: V a -> V a -> V a
    max a b = (P.pure P.max) P.<*> a P.<*> b

    min :: V a -> V a -> V a 
    min a b = (P.pure P.min) P.<*> a P.<*> b

infix 4 <
infix 4 <=
infix 4 >
infix 4 >= 

--
-- Integers
--
type VInt = V P.Int 

class P.Num a => VNum a where
    (+) :: V a -> V a -> V a 
    x + y = (P.pure (P.+)) P.<*> x P.<*> y

    (*) :: V a -> V a -> V a 
    x * y = (P.pure (P.*)) P.<*> x P.<*> y

    (-) :: V a -> V a -> V a 
    x - y = (P.pure (P.-)) P.<*> x P.<*> y  

    negate :: V a -> V a
    negate x = (P.pure P.negate) P.<*> x

    abs :: V a -> V a
    abs x = (P.pure P.abs) P.<*> x

    signum :: V a -> V a
    signum x = (P.pure P.signum) P.<*> x

    fromInteger :: V P.Integer -> V a 
    fromInteger x = (P.pure P.fromInteger) P.<*> x

instance VNum P.Int

instance VEq P.Int

instance VOrd P.Int 

--(^.) :: (b -> c) -> (a -> b) -> a -> c
--(^.) f0 f1 x = f0 (f1 x) 
--infixr 9 ^.

{-
map' :: (a -> b) -> [a] -> [b]
map' _f _xs = 
    case _xs of
        [] -> []
        (y : _ys) -> (_f y) : (map' _f _ys)
-}

--filter' :: (a -> Bool) -> [a] -> [a]
--filter' _p _xs =
--    case _xs of
--        [] -> []
--        (y : _ys) -> if _p y 
--                   then y : (filter' _p _ys) 
--                    else (filter' _p _ys)
{-
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _f z _xs = 
    case _xs of
        [] -> z
        (y : _ys) -> foldr' _f (_f y z) _ys       

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _f z _xs = 
    case _xs of
        [] -> z
        (y : _ys) -> foldl' _f (_f z y) _ys
-}