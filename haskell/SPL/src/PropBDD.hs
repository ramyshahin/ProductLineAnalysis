-- PropBDD.hs
-- Ramy Shahin
-- May 7th 2017
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PropBDD(
    Prop,
    unsat,
    mkBDDVar,
    andBDD,
    orBDD,
    notBDD,
    implies,
    tt,
    ff,
    disj
) where

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

instance NFData DDNode where
    rnf (DDNode a) = a `seq` ()

newtype Prop = Prop {
    b :: DDNode
    }
    deriving (Generic, NFData)

instance Eq Prop where
    {-# INLINE (==) #-}
    (Prop b0) == (Prop b1) = (b0 == b1)

instance Ord Prop where
    (Prop b0) <= (Prop b1) = nodeReadIndex b0 <= nodeReadIndex b1

instance Hashable Prop where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s (Prop b) = hashWithSalt s b

instance Show Prop where
    {-# INLINE show #-}
    show (Prop b) =
        case (unsafePerformIO $ lookupBDD b) of
            Nothing -> show b
            Just b' -> b'

type Universe = [Prop]

htSize :: (Eq k, Hashable k) => HashTable k v -> IO Int 
htSize h = do
    xs <- H.toList h 
    return $ length xs 

var2index :: HashTable String Int
bdd2string :: HashTable DDNode String

getVars :: Prop -> IO [(String, Int)]
getVars _ = do 
    H.toList $! var2index

lookupVar :: String -> Int
lookupVar v = unsafePerformIO $ do
    i <- H.lookup var2index v
    case i of
        Nothing -> do 
            i' <- htSize var2index
            !d0 <- H.insert var2index v i' 
            return $ i'
        Just i' -> return i'

lookupBDD :: DDNode -> IO (Maybe String)
lookupBDD b = H.lookup bdd2string b

lookupBDD' :: DDNode -> String
lookupBDD' b = unsafePerformIO $ do
    s <- lookupBDD b
    case s of
        Nothing -> return (show b)
        Just x  -> return x

addBDD :: DDNode -> String -> String
addBDD b s = unsafePerformIO $ do
    x <- lookupBDD b
    case x of
        Nothing -> H.insert bdd2string b s >> return s
        Just x' -> return x'

manager = cuddInit
var2index = unsafePerformIO H.new
bdd2string = unsafePerformIO H.new

{-# INLINE newBDD #-}
newBDD :: DDNode -> String -> Prop
newBDD b s = unsafePerformIO $ do
    x <- lookupBDD b
    case x of
        Nothing -> H.insert bdd2string b s >> return (Prop b)
        Just x' -> return $ Prop b
         
mkBDDVar :: String -> Prop
mkBDDVar name = 
    let i = lookupVar name 
        r = ithVar manager i 
    in newBDD r name

mkUniverse :: [String] -> Universe
mkUniverse = map mkBDDVar  
    
{-# INLINE tt #-}
ttBDD = readOne manager
tt = newBDD ttBDD "TT"

{-# INLINE ff #-}
ffBDD = readLogicZero manager
ff = newBDD ffBDD "FF"

{-# INLINE andBDD #-}
andBDD :: Prop -> Prop -> Prop
andBDD p0@(Prop b0) p1@(Prop b1) = 
    if      p0 == ff then ff
    else if p1 == ff then ff
    else if p0 == tt then p1
    else if p1 == tt then p0
    else    let b = bAnd manager b0 b1 
                sb0 = lookupBDD' b0
                sb1 = lookupBDD' b1
            in  newBDD b ("(" ++ sb0 ++ " /\\ " ++ sb1 ++ ")")

{-# INLINE orBDD #-}
orBDD :: Prop -> Prop -> Prop
orBDD p0@(Prop b0) p1@(Prop b1) = 
    if      p0 == tt then tt
    else if p1 == tt then tt
    else if p0 == ff then p1
    else if p1 == ff then p0
    else    let b = bOr manager b0 b1
                sb0 = lookupBDD' b0
                sb1 = lookupBDD' b1
            in  newBDD b ("(" ++ sb0 ++ " \\/ " ++ sb1 ++ ")")

{-# INLINE notBDD #-}
notBDD :: Prop -> Prop
notBDD p@(Prop b) =
    if      p == tt then ff
    else if p == ff then tt
    else    let b' = bNot manager b
            in newBDD b' ("!" ++ lookupBDD' b)

neg :: Prop -> Prop 
neg = notBDD 

disj :: [Prop] -> Prop
disj = foldr orBDD ff

impl p q = disj[notBDD p, q] 

tautology :: Prop -> Bool
tautology p = p == tt

sat :: Prop -> Bool
sat p = p /= ff

unsat :: Prop -> Bool
unsat p = p == ff

implies :: Prop -> Prop -> Prop 
implies p q = (impl p q)
