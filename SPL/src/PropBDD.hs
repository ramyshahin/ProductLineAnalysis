-- PropBDD.hs
-- Ramy Shahin
-- May 7th 2017
module PropBDD where

import Cudd.Cudd

manager = cuddInit

type Prop = DDNode

mkUniverse :: [String] -> [Prop]
mkUniverse xs = map (ithVar manager) [0..(length xs)-1]

tt = readOne manager
ff = readLogicZero manager

neg = bNot manager

conj :: [Prop] -> Prop
conj =  foldr (bAnd manager) tt

disj :: [Prop] -> Prop
disj = foldr (bOr manager) ff

impl p q = disj[neg p, q] 

tautology :: Prop -> Bool
tautology p = p == tt

sat :: Prop -> Bool
sat p = p /= ff

unsat :: Prop -> Bool
unsat p = p == ff

implies :: Prop -> Prop -> Bool
implies p q = tautology (impl p q)