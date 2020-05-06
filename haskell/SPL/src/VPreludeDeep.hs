--{-# LANGUAGE NoImplicitPrelude #-}

module VPreludeDeep where

import SPL
import Prelude

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

--filter' :: (Var a -> Var Bool) -> [Var a] -> [Var a]
--filter' _p _xs  = let case0 __cntxt__ = ([] ^| __cntxt__)
--                      split0 __dummy__ = case __dummy__ of [] -> ()
--                      case1 __cntxt__ _ys y = liftedCond (_p (y /^ __cntxt__)) (\__cntxt__ -> (y /^ __cntxt__) ^: (filter' _p _ys)) (\__cntxt__ -> (filter' _p _ys))
--                      split1 __dummy__ = case __dummy__ of (y : _ys) -> (_ys, y) in liftedCase (_xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                      (y : _ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]
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
