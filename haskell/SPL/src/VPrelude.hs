{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module VPrelude where

import qualified Prelude as P
import SPL 

--
-- Booleans
--
type VBool = Var P.Bool 

(&&) :: VBool -> VBool -> VBool
a && b = ((P.&&) ^| ttPC) P.<*> a P.<*> b 

(||) :: VBool -> VBool -> VBool
a || b = ((P.||) ^| ttPC) P.<*> a P.<*> b 

not :: VBool -> VBool
not a = (P.not ^| ttPC) P.<*> a 

class P.Eq a => VEq a where
    (==) :: Var a -> Var a -> VBool
    a == b = ((P.==) ^| ttPC) P.<*> a P.<*> b 

    (/=) :: Var a -> Var a -> VBool
    a /= b = ((P./=) ^| ttPC) P.<*> a P.<*> b 

type VOrdering = Var P.Ordering 

class P.Ord a => VOrd a where 
    compare :: Var a -> Var a -> VOrdering
    compare x y = (P.compare ^| ttPC) P.<*> x P.<*> y

    (<) :: Var a -> Var a -> VBool
    a < b = ((P.<) ^| ttPC) P.<*> a P.<*> b

    (<=) :: Var a -> Var a -> VBool
    a <= b = ((P.<=) ^| ttPC) P.<*> a P.<*> b

    (>)  :: Var a -> Var a -> VBool
    a > b = ((P.>) ^| ttPC) P.<*> a P.<*> b

    (>=) :: Var a -> Var a -> VBool
    a >= b = ((P.>=) ^| ttPC) P.<*> a P.<*> b 

    max :: Var a -> Var a -> Var a
    max a b = (P.max ^| ttPC) P.<*> a P.<*> b

    min :: Var a -> Var a -> Var a 
    min a b = (P.min ^| ttPC) P.<*> a P.<*> b

infix 4 <
infix 4 <=
infix 4 >
infix 4 >= 

--
-- Integers
--
type VInt = Var P.Int 

class P.Num a => VNum a where
    (+) :: Var a -> Var a -> Var a 
    x + y = ((P.+) ^| ttPC) P.<*> x P.<*> y

    (*) :: Var a -> Var a -> Var a 
    x * y = ((P.*) ^| ttPC) P.<*> x P.<*> y

    (-) :: Var a -> Var a -> Var a 
    x - y = ((P.-) ^| ttPC) P.<*> x P.<*> y  

    negate :: Var a -> Var a
    negate x = (P.negate ^| ttPC) P.<*> x

    abs :: Var a -> Var a
    abs x = (P.abs ^| ttPC) P.<*> x

    signum :: Var a -> Var a
    signum x = (P.signum ^| ttPC) P.<*> x

    fromInteger :: Var P.Integer -> Var a 
    fromInteger x = (P.fromInteger ^| ttPC) P.<*> x

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