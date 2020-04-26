--{-# LANGUAGE NoImplicitPrelude #-}

module VPrelude where

import Prelude

(^.) :: (b -> c) -> (a -> b) -> a -> c
(^.) f0 f1 x = f0 (f1 x) 
infixr 9 ^.

map' :: (a -> b) -> [a] -> [b]
map' f xs = 
    case xs of
        [] -> []
        (y : ys) -> (f y) : (map' f ys)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs =
    case xs of
        [] -> []
        (y : ys) -> if p y 
                    then y : (filter' p ys) 
                    else (filter' p ys)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z xs = 
    case xs of
        [] -> z
        (y : ys) -> foldr' f (f y z) ys       

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = 
    case xs of
        [] -> z
        (y : ys) -> foldl' f (f z y) ys