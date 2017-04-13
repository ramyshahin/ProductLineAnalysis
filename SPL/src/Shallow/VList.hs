module Shallow.VList where

import SPL

type VList a = Var [a]

vNil :: Var [a]
vNil = mkVarT []

vCons :: Var a -> Var [a] -> Var [a]
vCons x xs = union defs undefs
    where   defs = (liftV2 (:)) x xs
            undefs = restrict (undefinedAt x) xs

mkVList :: [Var a] -> VList a
mkVList xs = foldr vCons vNil xs

vhead :: VList a -> Var a 
vhead = liftV head

vtail :: VList a -> VList a 
vtail = liftV tail

vnull :: VList a -> Var Bool
vnull = liftV null

vmap :: Var (a -> b) -> VList a -> VList b
vmap = liftV2 map