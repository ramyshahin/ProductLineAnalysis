-- Propositional Logic and SAT solving
-- Ramy Shahin
-- Jan 3rd 2017
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Prop where

import Z3.Base
import Data.List
import System.IO.Unsafe
import Control.Monad.IO.Class
import Data.HashTable.IO as H
import Data.Hashable
import GHC.Generics
import Generics.Deriving

type SATCache = BasicHashTable Prop Result

cacheSize = 1000

satCache :: SATCache 
satCache = unsafePerformIO $ H.newSized cacheSize

conf :: Config
conf = unsafePerformIO mkConfig
    
ctxt :: Context
ctxt = unsafePerformIO $ mkContext conf
    
solver :: Solver
solver = unsafePerformIO $ mkSimpleSolver ctxt

mkUniverse :: [String] -> [Prop]
mkUniverse as = 
    map atom as

data Prop = P AST String

instance Hashable Prop where
    hashWithSalt x (P _ s) = hashWithSalt x s 

instance Eq Prop where
    (P _ a1) == (P _ a2) = a1 == a2

instance Ord Prop where
    (P _ a1) < (P _ a2) = a1 < a2
    p1 <= p2 = p1 < p2 || p1 == p2

instance Show Prop where
    show (P _ s) = s 

mkProp :: AST -> String -> Prop
mkProp a s = P a s

tt = mkProp (unsafePerformIO (mkTrue ctxt)) "True"         
ff = mkProp (unsafePerformIO (mkFalse ctxt)) "False"

atom s = mkProp (unsafePerformIO (mkFreshBoolVar ctxt s)) s

neg (P a s) = mkProp (unsafePerformIO (mkNot ctxt a)) ("Not(" ++ s ++ ")")

conj ps'' = 
    if null ps then
        tt
    else if (length ps) == 1 then
        head ps
    else if any (ff ==) ps then
        ff
    else  mkProp (unsafePerformIO (mkAnd ctxt ps')) str
    where   ps = filter (tt /=) (nub ps'')
            sorted_ps = sort ps
            str = "And" ++ show sorted_ps
            ps' = map (\(P x _) -> x) sorted_ps

disj ps'' = 
    if null ps then
        ff
    else if (length ps) == 1 then
        head ps
    else if any (tt ==) ps then
        tt
    else  mkProp (unsafePerformIO (mkOr ctxt ps')) str
    where   ps = filter (ff /=) (nub ps'')
            sorted_ps = sort ps
            str = "Or" ++ show sorted_ps
            ps' = map (\(P x _) -> x) sorted_ps

impl p'@(P p sp) q'@(P q sq) = 
    if p' == ff then tt
    else if p' == tt then q'
    else if q' == tt then tt
    else mkProp (
        unsafePerformIO (mkImplies ctxt p q)) (sp ++ " => " ++ sq)
    

mkZ3Script :: AST ->IO ()
mkZ3Script a = solverAssertCnstr ctxt solver a

checkSAT :: Prop -> Result
checkSAT p@(P a _) = unsafePerformIO $ do
    r <- H.lookup satCache p
    case r of
        Just r' -> return r'
        Nothing -> do 
            solverPush ctxt solver
            solverAssertCnstr ctxt solver a
            s <- solverCheck ctxt solver
            solverPop ctxt solver 1
            H.insert satCache p s 
            return s

sat :: Prop -> Bool
sat p = (checkSAT p) == Sat

unsat :: Prop -> Bool
unsat p = (checkSAT p) == Unsat

tautology :: Prop -> Bool
tautology p = unsat (neg p)

implies :: Prop -> Prop -> Bool
implies x y = tautology (impl x y)

