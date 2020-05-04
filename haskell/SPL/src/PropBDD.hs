-- PropBDD.hs
-- Ramy Shahin
-- May 7th 2017
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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
import GHC.ForeignPtr
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

type HashTable k v = H.BasicHashTable k v

instance Hashable DDNode where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s d = hashWithSalt s (nodeReadIndex d)

data Prop' =
    TT
  | FF
  | Atom String
  | Neg Prop'
  | And Prop' Prop'
  | Or Prop' Prop'    
    deriving (Eq, Generic, NFData) 

instance NFData DDNode where
    rnf a = seq a ()

instance Show Prop' where 
    show p = case p of 
        TT -> "True"
        FF -> "False"
        Atom s -> s
        Neg p' -> "~" ++ (show p')
        And p1 p2 -> "(" ++ (show p1) ++ " /\\ " ++ (show p2) ++ ")"
        Or  p1 p2 -> "(" ++ (show p1) ++ " \\/ " ++ (show p2) ++ ")"

instance Hashable Prop' where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s p = hashWithSalt s (show p)

data Prop = Prop {
    p :: Prop',
    b :: DDNode--,
    --s :: String
    }
    deriving (Generic, NFData)

instance Eq Prop where
    (Prop _ b0) == (Prop _ b1) = (b0 == b1)

instance Hashable Prop where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s (Prop _ b) = hashWithSalt s b

instance Show Prop where
    {-# INLINE show #-}
    show (Prop p _) = show p

type Universe = [Prop]

--prop2bdd :: HashTable Prop DDNode
bdd2prop :: HashTable DDNode Prop

--propTable :: HashTable String Prop

htSize :: (Eq k, Hashable k) => H.BasicHashTable k v -> IO Int 
htSize h = do
    xs <- H.toList h 
    return $ length xs 

var2index :: HashTable String Int

getVars :: [(String, Int)]
getVars = unsafePerformIO $ H.toList var2index

lookupVar :: String -> Int
lookupVar v = unsafePerformIO $ do
    i <- H.lookup var2index v
    case i of
        Nothing -> do 
            i' <- htSize var2index
            !d0 <- H.insert var2index v i' 
            return i'
        Just i' -> return i'

manager = cuddInit
--prop2bdd = unsafePerformIO H.new 
bdd2prop = unsafePerformIO H.new 
var2index = unsafePerformIO H.new
--propTable = unsafePerformIO H.new

{-# INLINE newBDD #-}
newBDD :: Prop' -> DDNode -> Prop
newBDD p d = unsafePerformIO $ do
    --let s = show p
    let ret = Prop p d --s
    --p' <- H.lookup propTable s
    --case p' of
        --Nothing -> do  
    p'' <- H.lookup bdd2prop d
    case p'' of
                        Nothing -> do
                                    !d0 <- H.insert bdd2prop d ret
                                    -- !d1 <- H.insert propTable s ret
                                    --trace ("inserted: " ++ (show p)) (return p)
                                    return $ ret
                        Just _p -> return $ _p
        --Just p'' -> return p''
         
mkBDDVar :: String -> Prop
mkBDDVar name = 
    let i = lookupVar name 
        r = ithVar manager i 
    in newBDD (Atom name) r

mkUniverse :: [String] -> Universe
mkUniverse = map mkBDDVar  
    
--{-# NOINLINE tt #-}
tt = newBDD TT (readOne manager)
ff = newBDD FF (readLogicZero manager)

{-
andBDD' :: Prop -> Prop -> Prop
andBDD' a b =
    let a' = unsafePerformIO $ H.lookup prop2bdd a 
        b' = unsafePerformIO $ H.lookup prop2bdd b
    in  case (a', b') of 
        (Just a'', Just b'') -> newBDD (And a b) $ bAnd manager a'' b''
        (Just _, Nothing) -> error $ "Can not find RHS: " ++ (show b)
        (Nothing, Just _) -> error $ "Can not find LHS: " ++ (show a)
        _ -> error $ "Can not find props: " ++ (show a) ++ " " ++ (show b)
-}

andBDD :: Prop -> Prop -> Prop
andBDD p0'@(Prop p0 b0) p1'@(Prop p1 b1) = 
    if      p0 == TT then p1'
    else if p1 == TT then p0'
    else    newBDD (And p0 p1) $ bAnd manager b0 b1 
    
    --let p  = And p0 p1
    --    b' = unsafePerformIO $ H.lookup prop2bdd p
    --in  case b' of
    --    Just _ -> p
    --    _       -> andBDD' a b

    {-
orBDD' :: Prop -> Prop -> Prop
orBDD' a b = 
    let a' = unsafePerformIO $ H.lookup prop2bdd a 
        b' = unsafePerformIO $ H.lookup prop2bdd b
    in  case (a', b') of 
        (Just a'', Just b'') -> newBDD (Or a b) $ bOr manager a'' b''
        _ -> error $ "Can not find props: " ++ (show a) ++ " " ++ (show b)
-}

orBDD :: Prop -> Prop -> Prop
orBDD p0'@(Prop p0 b0) p1'@(Prop p1 b1) = 
    if      p0 == FF then p1'
    else if p1 == FF then p0'
    else newBDD (Or p0 p1) $ bOr manager b0 b1
{-
    let p  = Or a b
        b' = unsafePerformIO $ H.lookup prop2bdd p
    in  case b' of
        Just _ -> p
        _       -> orBDD' a b
-}

{-
notBDD' :: Prop -> Prop 
notBDD' a = 
    let a' = unsafePerformIO $ H.lookup prop2bdd a 
    in  case a' of 
        Just a' -> newBDD (Neg a) $ bNot manager a'
        _ -> error $ "Can not find prop: " ++ (show a)
-}

notBDD :: Prop -> Prop
notBDD (Prop p0 b0) =
    if      p0 == TT then ff
    else if p0 == FF then tt
    else newBDD (Neg p0) $ bNot manager b0

{-
    let p = Neg a
        b = unsafePerformIO $ H.lookup prop2bdd p
    in  case b of
        Just _  -> p
        _       -> notBDD' a
-}

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
