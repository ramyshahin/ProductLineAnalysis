--{-# LANGUAGE NoImplicitPrelude #-}

module VPreludeDeep where

import SPL
import Prelude
import Debug.Trace

(^.) :: (Var b -> Var c) -> (Var a -> Var b) -> Var a -> Var c
(^.) f0 f1 x  = f0 (f1 x) 
infixr 9 ^.

map' :: (Var a -> Var b) -> Var [a] -> Var [b]
map' f xs  = let case0 = (mkVarT [])
                 split0 __dummy__ = case __dummy__ of [] -> ()
                 case1 y ys = (f y) ^: (map' f ys)
                 split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of (y : ys) -> 1
                                                                                                                              _ -> 0
                                                                                                                              ) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]

filter' :: Show a => (Var a -> Var Bool) -> Var [a] -> Var [a]
filter' p xs  = let case0 = trace "NilCase!!!!!" (mkVarT [])
                    split0 __dummy__ = case __dummy__ of [] -> ()
                    case1 y ys = 
                        let fs = (filter' p ys)
                            ret = liftedCond 
                                  (let ret = p y
                                   in --trace ("Condition-----" ++ (show ret)) 
                                      ret) 
                                  (let ret = (y ^: fs)
                                   in --trace ("\t------True branch-----" ++ (show ret)) 
                                      ret)
                                  (let ret = fs
                                   in --trace ("\t------False branch-----" ++ (show ret)) 
                                      ret)
                        in  --trace ("\ty  : " ++ (show y)) $
                            --trace ("\tys : " ++ (show ys)) $
                            --trace ("\tcase1-ret: " ++ (show ret)) $
                            ret
                    split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) 
                    ret = liftedCase (xs) (\__dummy__ -> case __dummy__ of (y : ys) -> 1 
                                                                           _ -> 0) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]
                in --trace ("Filter-out: " ++ (show ret)) ret
                    ret

foldr' :: (Var a -> Var b -> Var b) -> Var b -> Var [a] -> Var b
foldr' f z xs  = let case0 = z
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 y ys = foldr' f (f y z) ys
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) 
                 in trace "foldr'" $
                     liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                      (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]       

foldl' :: (Var b -> Var a -> Var b) -> Var b -> Var [a] -> Var b
foldl' f z xs  = let case0 = z
                     split0 __dummy__ = case __dummy__ of [] -> ()
                     case1 y ys = foldl' f (f z y) ys
                     split1 __dummy__ = case __dummy__ of (y : ys) -> (y, ys) in liftedCase (xs) (\__dummy__ -> case __dummy__ of [] -> 0
                                                                                                                                  (y : ys) -> 1) [(uncurry0 case0) . (liftV split0), (uncurry2 case1) . (liftV split1)]
