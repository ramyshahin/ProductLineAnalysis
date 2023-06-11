--{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module VPrelude where

import qualified Prelude as P
import SPL 

--
-- Integers
--
type Int = Var P.Int 

instance P.Num Int where
    x + y = ((P.+) ^| ttPC) P.<*> x P.<*> y
    x * y = ((P.*) ^| ttPC) P.<*> x P.<*> y
    x - y = ((P.-) ^| ttPC) P.<*> x P.<*> y  
    abs x = (P.abs ^| ttPC) P.<*> x
    signum x = (P.signum ^| ttPC) P.<*> x
    fromInteger x = (P.fromInteger ^| ttPC) P.<*> (x ^| ttPC)

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