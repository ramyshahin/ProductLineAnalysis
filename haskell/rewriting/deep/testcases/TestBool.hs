{-# LANGUAGE NoImplicitPrelude #-}

module TestBool where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data Bool = False | True

not :: Bool -> Bool
not x =
    case x of
        False -> True
        True -> False

ite :: Bool -> a -> a -> a
ite c x y =
    case c of 
        True -> x
        False -> y

(&&) :: Bool -> Bool -> Bool
x && y = ite x y False

(||) :: Bool -> Bool -> Bool 
x || y = ite x True y
