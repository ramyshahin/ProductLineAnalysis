-- PropBDD.hs
-- Ramy Shahin
-- May 7th 2017
module PropBDD where

import Cudd.Cudd

manager = cuddInit

type Prop = DDNode
type Universe = [Prop]

mkUniverse :: [String] -> Universe
mkUniverse xs = map (\(s,r) -> (ithVar manager r)) (zip xs [0..])

queryOrUpdate :: Universe -> String -> (Universe, Prop)
queryOrUpdate [] s = (u', p)
    where   u' = mkUniverse [s]
            p  = head u' 

--queryOrUpdate u@((s',p'):u') s =
--    if s == s' 
--    then (u,p')
--    else ((s',p'):u'', p'')
--         where (u'',p'') = queryOrUpdate u' s
         
{-# NOINLINE tt #-}
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
