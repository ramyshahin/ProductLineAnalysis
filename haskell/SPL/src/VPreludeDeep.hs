--{-# LANGUAGE NoImplicitPrelude #-}

module VPreludeDeep where

import SPL
import Prelude

--(^.) :: (b -> c) -> (a -> b) -> a -> c
--(^.) f0 f1 x = f0 (f1 x) 
--infixr 9 ^.

map' :: Context -> (Context -> Var a -> Var b) -> Var [a] -> Var [b]
map' __cntxt__ f xs = let case0 __cntxt__ = ([] ^| __cntxt__)
                          split0 __dummy__ = case __dummy__ of [] -> ()
                          case1 __cntxt__ y ys = (f __cntxt__ y) ^: (map' __cntxt__ f ys)
                          split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                       (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]

filter' :: Context -> (Context -> Var a -> Var Bool) -> Var [a] -> Var [a]
filter' __cntxt__ p xs = let case0 __cntxt__ = ([] ^| __cntxt__)
                             split0 __dummy__ = case __dummy__ of [] -> ()
                             case1 __cntxt__ y ys = liftedCond __cntxt__ (p __cntxt__ y) (\__cntxt__ -> y ^: (filter' __cntxt__ p ys)) (\__cntxt__ -> (filter' __cntxt__ p ys))
                             split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                          (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]

foldr' :: Context -> (Context -> Var a -> Var b -> Var b) -> Var b -> Var [a] -> Var b
foldr' __cntxt__ f z xs = let case0 __cntxt__ = z
                              split0 __dummy__ = case __dummy__ of [] -> ()
                              case1 __cntxt__ y ys = foldr' __cntxt__ f (f __cntxt__ y z) ys
                              split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                           (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]       

foldl' :: Context -> (Context -> Var b -> Var a -> Var b) -> Var b -> Var [a] -> Var b
foldl' __cntxt__ f z xs = let case0 __cntxt__ = z
                              split0 __dummy__ = case __dummy__ of [] -> ()
                              case1 __cntxt__ y ys = foldl' __cntxt__ f (f __cntxt__ z y) ys
                              split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                           (y : ys) -> 1) [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry2 (case1 __cntxt__)) . (liftV split1)]
