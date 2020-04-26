--{-# LANGUAGE NoImplicitPrelude #-}

module VPreludeDeep where

import SPL
import Prelude

(^.) :: (Var b -> Var c) -> (Var a -> Var b) -> Var a -> Var c
(^.) f0 f1 x  = f0 (f1 x) 
infixr 9 ^.

map' :: (Var a -> Var b) -> Var [a] -> Var [b]
map' f xs  = let case0 = mkVarT []
                 split0 __dummy__ = case __dummy__ of [] -> ()
                 case1 y ys = (f y) ^: (map' f ys)
                 split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                              (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]

filter' :: (Var a -> Var Bool) -> Var [a] -> Var [a]
filter' p xs  = let case0 = mkVarT []
                    split0 __dummy__ = case __dummy__ of [] -> ()
                    case1 y ys = liftedCond (p y) (y ^: (filter' p ys)) ((filter' p ys))
                    split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                 (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]

foldr' :: (Var a -> Var b -> Var b) -> Var b -> Var [a] -> Var b
foldr' f z xs  = let case0 = z
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 y ys = foldr' f (f y z) ys
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                  (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]       

foldl' :: (Var b -> Var a -> Var b) -> Var b -> Var [a] -> Var b
foldl' f z xs  = let case0 = z
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 y ys = foldl' f (f z y) ys
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                  (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]
