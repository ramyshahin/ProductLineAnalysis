{-# LANGUAGE NoImplicitPrelude #-}

module Bool where
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

and :: Bool -> Bool -> Bool
and x y = if x then y else False

or :: Bool -> Bool -> Bool 
or x y = if x then True else y
