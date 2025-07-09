{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module VPrelude (
    module VPrelude,
    module ShallowTypes,
    module VBool,
    module VList
)where

import qualified Prelude as P
-- #ifdef COMPACTION
-- import SPLOpt
-- #else
import SPL 
-- #endif

import ShallowTypes 
import VBool
import VList
--import VList

--
-- Booleans
--

(&&) :: VBool -> VBool -> VBool
a && b = and a b

(||) :: VBool -> VBool -> VBool
a || b = or a b

{-
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
-}

--
-- Integers
--

--type VInt = V Int 

{-
instance VEq P.Int

instance VOrd P.Int 
-}

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

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

{-
match_ :: (VClass a) => [SumOption a] -> [a]
match_ [] = []
match_ (x : xs) =
    case x of
        Absent -> match_ xs
        Present (v, pc) -> (restrict pc v) : match_ xs
-}

__cntxt__ :: PresenceCondition
__cntxt__ = allConfigs
