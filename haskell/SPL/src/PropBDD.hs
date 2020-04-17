-- PropBDD.hs
-- Ramy Shahin
-- May 7th 2017
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module PropBDD where

import Cudd.Cudd
import Data.Hashable
import qualified Data.HashTable.IO as H
import Control.Monad.ST
import qualified Data.List as L 
import Debug.Trace
import Data.STRef
import Control.Monad
import System.IO.Unsafe

type HashTable k v = H.BasicHashTable k v

instance Hashable DDNode where
    hashWithSalt s d = hashWithSalt s (show d)

data Prop =
    TT
  | FF
  | Atom String
  | Neg Prop
  | And Prop Prop
  | Or Prop Prop    
    deriving (Eq) 

instance Show Prop where 
    show p = case p of 
        TT -> "True"
        FF -> "False"
        Atom s -> s
        Neg p' -> "~" ++ (show p')
        And p1 p2 -> "(" ++ (show p1) ++ " /\\ " ++ (show p2) ++ ")"
        Or  p1 p2 -> "(" ++ (show p1) ++ " \\/ " ++ (show p2) ++ ")"

instance Hashable Prop where
    hashWithSalt s p = hashWithSalt s (show p)

type Universe = [Prop]

prop2bdd :: HashTable Prop DDNode
bdd2prop :: HashTable DDNode Prop

htSize :: (Eq k, Hashable k) => H.BasicHashTable k v -> IO Int 
htSize h = do
    xs <- H.toList h 
    return $ length xs 

var2index :: HashTable String Int
index2var :: HashTable Int String

getVars :: [(Int, String)]
getVars = unsafePerformIO $ H.toList index2var

lookupVar :: String -> Int
lookupVar v = unsafePerformIO $ do
    i <- H.lookup var2index v
    case i of
        Nothing -> do 
            i' <- htSize var2index
            !d0 <- H.insert var2index v i' 
            !d1 <- H.insert index2var i' v
            return i'
        Just i' -> return i'

manager = cuddInit
prop2bdd = unsafePerformIO H.new 
bdd2prop = unsafePerformIO H.new 
var2index = unsafePerformIO H.new
index2var = unsafePerformIO H.new

newBDD :: Prop -> DDNode -> Prop
newBDD p d = unsafePerformIO $ do
    p' <- H.lookup bdd2prop d
    case p' of 
            Nothing -> do
                        !d0 <- H.insert bdd2prop d p
                        !d1 <- H.insert prop2bdd p d
                        --trace ("inserted: " ++ (show p)) (return p)
                        return p
            Just p'' -> return p''   

mkBDDVar :: String -> Prop
mkBDDVar name = 
    let i = lookupVar name 
        r = ithVar manager i 
    in newBDD (Atom name) r

mkUniverse :: [String] -> Universe
mkUniverse = map mkBDDVar  
    
{-# NOINLINE tt #-}
tt = newBDD TT (readOne manager)
ff = newBDD FF (readLogicZero manager)

andBDD :: Prop -> Prop -> Prop
andBDD a b =
    let a' = unsafePerformIO $ H.lookup prop2bdd a 
        b' = unsafePerformIO $ H.lookup prop2bdd b
    in  case (a', b') of 
        (Just a'', Just b'') -> newBDD (And a b) $ bAnd manager a'' b''
        (Just _, Nothing) -> error $ "Can not find RHS: " ++ (show b)
        (Nothing, Just _) -> error $ "Can not find LHS: " ++ (show a)
        _ -> error $ "Can not find props: " ++ (show a) ++ " " ++ (show b)

orBDD :: Prop -> Prop -> Prop
orBDD a b = 
    let a' = unsafePerformIO $ H.lookup prop2bdd a 
        b' = unsafePerformIO $ H.lookup prop2bdd b
    in  case (a', b') of 
        (Just a'', Just b'') -> newBDD (Or a b) $ bOr manager a'' b''
        _ -> error $ "Can not find props: " ++ (show a) ++ " " ++ (show b)

notBDD :: Prop -> Prop 
notBDD a = 
    let a' = unsafePerformIO $ H.lookup prop2bdd a 
    in  case a' of 
        Just a' -> newBDD (Neg a) $ bNot manager a'
        _ -> error $ "Can not find prop: " ++ (show a)

neg :: Prop -> Prop 
neg = notBDD 

conj :: [Prop] -> Prop
conj =  foldl andBDD tt

disj :: [Prop] -> Prop
disj = foldr orBDD ff

impl p q = disj[notBDD p, q] 

tautology :: Prop -> Bool
tautology p = p == tt

sat :: Prop -> Bool
sat p = p /= ff

unsat :: Prop -> Bool
unsat p = p == ff

implies :: Prop -> Prop -> Bool
implies p q = tautology (impl p q)
