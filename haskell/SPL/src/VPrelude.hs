--{-# LANGUAGE NoImplicitPrelude #-}

module VPrelude where

import Prelude

(^.) :: (b -> c) -> (a -> b) -> a -> c
(^.) f0 f1 x = f0 (f1 x) 
infixr 9 ^.

map' :: (a -> b) -> [a] -> [b]
map' _f xs = 
    case xs of
        [] -> []
        (y : ys) -> (_f y) : (map' _f ys)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _p xs =
    case xs of
        [] -> []
        (y : ys) -> if _p y 
                    then y : (filter' _p ys) 
                    else (filter' _p ys)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _f z xs = 
    case xs of
        [] -> z
        (y : ys) -> foldr' _f (_f y z) ys       

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _f z xs = 
    case xs of
        [] -> z
        (y : ys) -> foldl' _f (_f z y) ys