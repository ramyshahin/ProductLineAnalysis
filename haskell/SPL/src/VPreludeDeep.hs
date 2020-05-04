--{-# LANGUAGE NoImplicitPrelude #-}

module VPreludeDeep where

import SPL
import Prelude

(^.) :: (Var b -> Var c) -> (Var a -> Var b) -> Var a -> Var c
(^.) f0 f1 x  = f0 (f1 x) 
infixr 9 ^.

map' :: (Var a -> Var b) -> Var [a] -> Var [b]
map' _f xs  = let case0 __cntxt__ = ([] ^| __cntxt__)
                  split0 __dummy__ = case __dummy__ of [] -> ()
                  case1 __cntxt__ y ys = (_f (y /^ __cntxt__)) ^: (map' _f (ys /^ __cntxt__))
                  split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                               (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]

filter' :: (Var a -> Var Bool) -> Var [a] -> Var [a]
filter' _p xs  = let case0 __cntxt__ = ([] ^| __cntxt__)
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 __cntxt__ y ys = liftedCond (_p (y /^ __cntxt__)) (\__cntxt__ -> (y /^ __cntxt__) ^: (filter' _p (ys /^ __cntxt__))) (\__cntxt__ -> (filter' _p (ys /^ __cntxt__)))
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                  (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]

foldr' :: (Var a -> Var b -> Var b) -> Var b -> Var [a] -> Var b
foldr' _f z xs  = let case0 __cntxt__ = (z /^ __cntxt__)
                      split0 __dummy__ = case __dummy__ of [] -> ()
                      case1 __cntxt__ y ys = foldr' _f (_f (y /^ __cntxt__) (z /^ __cntxt__)) (ys /^ __cntxt__)
                      split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                   (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]       

foldl' :: (Var b -> Var a -> Var b) -> Var b -> Var [a] -> Var b
foldl' _f z xs  = let case0 __cntxt__ = (z /^ __cntxt__)
                      split0 __dummy__ = case __dummy__ of [] -> ()
                      case1 __cntxt__ y ys = foldl' _f (_f (z /^ __cntxt__) (y /^ __cntxt__)) (ys /^ __cntxt__)
                      split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                   (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]
