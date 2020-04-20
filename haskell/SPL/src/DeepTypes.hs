-- Deeply lifted primitive data types
module DeepTypes where

{-
import SPL

data List' a =
    Nil'
  | (Var a) :^ List' a

-- lifted primitive functions
head' :: List' a -> Var a
head' (x :^ xs) = x

tail' :: List' a -> List' a
tail' Nil'      = Nil'
tail' (x :^ xs) = xs 

null' :: List' a -> Var Bool
null' Nil' = mkVarT True
null' _    = mkVarT False

data Tuple' a b = 
    Tuple' (Var a) (Var b)

fst' :: Tuple' a b -> Var a
fst' (Tuple' a _)= a

snd' :: Tuple' a b -> Var b
snd' (Tuple' _ b) = b 
-}