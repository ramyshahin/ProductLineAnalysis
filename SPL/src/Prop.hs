-- Propositional Logic and SAT solving
-- Ramy Shahin
-- Jan 3rd 2017
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Prop where

import Z3.Base
import Z3.Monad(Z3, assert, evalZ3, evalZ3With, check, local)
import Z3.Opts
import Data.Vector as V(Vector, (!), fromList, empty, length, snoc, findIndex)
import Data.List
import System.IO.Unsafe
import Control.Monad.IO.Class
import Data.HashTable.IO as H
import Data.Hashable
import GHC.Generics
import Generics.Deriving

type SATCache = BasicHashTable Prop Result

--type Universe = (BasicHashTable Int Prop)

satCache :: SATCache 
satCache = (unsafePerformIO H.new)

--varASTs :: Universe
--varASTs = (unsafePerformIO H.new)
conf :: Config
conf = unsafePerformIO mkConfig
    
cntxt :: Context
cntxt = unsafePerformIO $ mkContext conf
    
mkUniverse :: [String] -> [Prop]
mkUniverse as = 
    map atom as

    --let _ = unsafePerformIO $ mapM (\i -> H.insert varASTs i (Atom (xs ! i))) [0..(V.length xs)-1] 
    --return ()
{-
queryOrUpdate :: Universe -> String -> (Universe, Int)
queryOrUpdate u@(Universe as ht) q =
    let i = V.findIndex (== q) as
    in  case i of
            Nothing -> (Universe (V.snoc as q) ht, V.length as)
            Just i' -> (u, i')
  -}

{-
data Prop =
    T
  | F
  | Atom  AST
  | Not'  Prop
  | Conj' [Prop]
  | Disj' [Prop]
  | Impl' Prop Prop
  | Iff'  Prop Prop
  deriving (Eq, Generic)

instance Hashable Prop


hasConflict :: [Prop] -> Bool
--hasConflict ps = any (\(Not' p) -> (any (\p' -> p == p')) ps) ps
hasConflict _ = False

simplify :: Prop -> Prop
simplify p =
    case p of
        Not' p' -> 
            case p' of
                T -> F
                F -> T
                Not' p'' -> p'' 
                _ -> Not' p'
        Conj' ps -> let ps'' = filter (\p -> p /= T) ps
                        ps'  = nub $ foldr (++) [] (map (\p -> case p of
                                                                  Conj' ps' -> ps'
                                                                  _ -> [p]) ps'')
                    in case ps' of
                        [] -> T
                        [p] -> p
                        ps -> if (any (== F) ps') || (hasConflict ps) then F else Conj' ps'

        Disj' ps -> let ps'' = filter (\p -> p /= F) ps
                        ps'  = nub $ foldr (++) [] (map (\p -> case p of
                                                                  Disj' ps' -> ps'
                                                                  _ -> [p]) ps'')
                    in case ps' of
                        [] -> F
                        [p] -> p
                        _ -> if ((any (== T) ps') || (hasConflict ps)) then T else Disj' ps'
        _ -> p
-}

data Prop = P AST String

instance Hashable Prop where
    hashWithSalt x (P _ s) = hashWithSalt x s 

instance Eq Prop where
    (P a1 _) == (P a2 _) = a1 == a2

instance Ord Prop where
    (P a1 _) < (P a2 _) = a1 < a2
    p1 <= p2 = p1 < p2 || p1 == p2

instance Show Prop where
    show (P _ s) = s 

{-
mkZ3Formula :: Prop -> Z3 AST
mkZ3Formula p =
    case p of
        T           -> mkTrue
        F           -> mkFalse
        Atom i      -> return (getVar i)
        Not' p'      -> mkNot =<< (mkZ3Formula p')
        Conj' ps     -> mkAnd =<< (mapM mkZ3Formula ps)
        Disj' ps     -> mkOr  =<< (mapM mkZ3Formula ps)
        Impl' p1 p2  -> do
                        p1' <- mkZ3Formula p1
                        p2' <- mkZ3Formula p2
                        mkImplies p1' p2'
        Iff' p1 p2   -> do
                        p1' <- mkZ3Formula p1
                        p2' <- mkZ3Formula p2
                        mkIff p1' p2'
-}

mkProp :: IO AST -> String -> Prop
mkProp a s = unsafePerformIO $ do
    a' <- a
    return (P a' s)

tt = mkProp (mkTrue cntxt) "True"         
ff = mkProp (mkFalse cntxt) "False"

atom s = mkProp (mkFreshBoolVar cntxt s) s

neg (P a s) = mkProp (mkNot cntxt a) ("Not(" ++ s ++ ")")

conj ps = 
    let sorted_ps = sort ps
        str = "And" ++ show sorted_ps
        ps' = map (\(P x _) -> x) sorted_ps
    in  mkProp (mkAnd cntxt ps') str

disj ps = 
    let sorted_ps = sort ps
        str = "Or" ++ show sorted_ps
        ps' = map (\(P x _) -> x) sorted_ps
    in  mkProp (mkOr cntxt ps') str

impl (P p sp) (P q sq) = mkProp (mkImplies cntxt p q) (sp ++ " => " ++ sq)
    
--iff p q = simplify(Iff' p q)

{-
showPropList :: [Prop] -> String
showPropList ps = 
    case ps of
        []  -> ""
        [p] -> (show p) 
        p1 : p2 : ps' -> (show p1) ++ ", " ++ showPropList (p2 : ps')

getVar :: Int -> Prop
getVar i = 
    let (Just s) = unsafePerformIO $ H.lookup varASTs i
    in  s

instance Show Prop where
    show prop = 
        case prop of
            T            -> "True"
            F            -> "False"
            Atom i       -> show $ getVar i
            Not' p       -> "Not(" ++ show p ++ ")"
            Conj' ps     -> "And(" ++ showPropList ps ++ ")"
            Disj' ps     -> "Or("  ++ showPropList ps ++ ")"
            Impl' p1 p2  -> "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
            Iff'  p1 p2  -> "(" ++ show p1 ++ " <=> " ++ show p2 ++ ")"

instance Ord Prop where
    compare T p = case p of
                    T -> EQ
                    _ -> LT
    compare F p = case p of
                    T -> GT
                    F -> EQ
                    _ -> LT
    compare (Atom i) p = case p of
                            T -> GT
                            F -> GT
                            (Atom j) -> compare i j
                            _ -> LT
    compare (Not' p1) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _) -> GT
                            (Not' p2) -> compare p1 p2
                            _ -> LT
    compare (Conj' a) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _) -> GT
                            (Not' _) -> GT
                            (Conj' b) -> compare a b
                            _ -> LT
    compare (Disj' a) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _) -> GT
                            (Not' _) -> GT
                            (Conj' _) -> GT
                            (Disj' b) -> compare a b
-}
{-
maxUniverse :: Universe -> Universe -> Universe
maxUniverse u1@(Universe as1 _) u2@(Universe as2 _) =
    let l1 = V.length as1
        l2 = V.length as2
    in
        if (l1 >= l2) then u1 else u2

getUniverse :: Prop -> Universe
getUniverse p = 
    case p of
        T -> emptyUniverse
        F -> emptyUniverse
        Atom u i -> u
        Not' p' -> getUniverse p'
        Conj' ps -> foldr (maxUniverse . getUniverse) emptyUniverse ps
        Disj' ps -> foldr (maxUniverse . getUniverse) emptyUniverse ps
        Impl' p1 p2 -> maxUniverse (getUniverse p1) (getUniverse p2)
        Iff'  p1 p2 -> maxUniverse (getUniverse p1) (getUniverse p2)
-}

mkZ3Script :: AST -> Z3 ()
mkZ3Script a = do
    assert a

checkSAT :: Prop -> Result
checkSAT p@(P a _) = unsafePerformIO $ do
    r <- H.lookup satCache p
    res <- case r of
                Just r' -> return r'
                Nothing -> do {s <- evalZ3With Nothing stdOpts (((mkZ3Script a) >> check)); H.insert satCache p s; return s}
    return res

sat :: Prop -> Bool
sat p = (checkSAT p) == Sat

unsat :: Prop -> Bool
unsat p = (checkSAT p) == Unsat

tautology :: Prop -> Bool
tautology p = unsat (neg p)

implies :: Prop -> Prop -> Bool
implies x y = tautology (impl x y)

-- Context indices are dummy, used here just to make sure 
-- the functions are actually called rather than lazily skipped
{-
localCtxt_ :: Prop -> a -> Z3 a
localCtxt_ p x =
    do 
        (mkZ3Script (getUniverse p) p)
        push
        let !r = x
        pop 1
        return r

localCtxt :: Prop -> a -> a
localCtxt p x = unsafePerformIO $ evalZ3 (localCtxt_ p x)
-}