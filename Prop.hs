-- Propositional Logic and SAT solving
-- Ramy Shahin
-- Jan 3rd 2017
module Prop where

import Z3.Monad
import Data.Vector as V(Vector, (!), fromList, empty, length)
import Data.List

type Universe = Vector String

mkUniverse :: [String] -> Universe
mkUniverse = fromList

data Prop =
    T
  | F
  | Atom Universe Int
  | Not' Prop
  | Conj' [Prop]
  | Disj' [Prop]
  | Impl' Prop Prop
  | Iff'  Prop Prop
  deriving Eq

neg T = F
neg F = T
neg p = Not' p

conj'' [] = T
conj'' [p] = p
conj'' ps  = Conj' ps
conj' ps = if (any (\p -> p == F) ps) then F else conj'' ps
conj  ps = conj' (filter (\p -> p /=T) ((map head . group . sort) ps))

disj'' [] = F
disj'' [p] = p
disj'' ps = Disj' ps
disj' ps = if (any (\p -> p == T) ps) then T else disj'' ps
disj ps =  conj' (filter (\p -> p /= F) ((map head . group . sort) ps)) 

{-
impl F p = T
impl p T = T
impl p q = Impl' p q

iff T p = p
iff p T = p
iff F p = neg p
iff p F = neg p
iff p q = Iff' p q
-}

showPropList :: [Prop] -> String
showPropList ps = 
    case ps of
        []  -> ""
        [p] -> (show p) 
        p1 : p2 : ps' -> (show p1) ++ ", " ++ showPropList (p2 : ps')

instance Show Prop where
    show prop = 
        case prop of
            T           -> "True"
            F           -> "False"
            Atom u i    ->  u ! i
            Not' p       -> "Not(" ++ show p ++ ")"
            Conj' ps     -> "And(" ++ showPropList ps ++ ")"
            Disj' ps     -> "Or("  ++ showPropList ps ++ ")"
            --Impl' p1 p2  -> "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
            --Iff'  p1 p2  -> "(" ++ show p1 ++ " <=> " ++ show p2 ++ ")"

instance Ord Prop where
    compare T p = case p of
                    T -> EQ
                    _ -> LT
    compare F p = case p of
                    T -> GT
                    F -> EQ
                    _ -> LT
    compare (Atom _ i) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _ j) -> compare i j
                            _ -> LT
    compare (Not' p1) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _ _) -> GT
                            (Not' p2) -> compare p1 p2
                            _ -> LT
    compare (Conj' a) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _ _) -> GT
                            (Not' _) -> GT
                            (Conj' b) -> compare a b
                            _ -> LT
    compare (Disj' a) p = case p of
                            T -> GT
                            F -> GT
                            (Atom _ _) -> GT
                            (Not' _) -> GT
                            (Conj' _) -> GT
                            (Disj' b) -> compare a b

maxUniverse :: Universe -> Universe -> Universe
maxUniverse u1 u2 =
    let l1 = V.length u1
        l2 = V.length u2
    in
        if (l1 >= l2) then u1 else u2

getUniverse :: Prop -> Universe
getUniverse p = 
    case p of
        T -> empty
        F -> empty
        Atom u i -> u
        Not' p' -> getUniverse p'
        Conj' ps -> foldr (maxUniverse . getUniverse) empty ps
        Disj' ps -> foldr (maxUniverse . getUniverse) empty ps
        --Impl' p1 p2 -> maxUniverse (getUniverse p1) (getUniverse p2)
        --Iff'  p1 p2 -> maxUniverse (getUniverse p1) (getUniverse p2)

mkZ3Formula :: Vector AST -> Prop -> Z3 AST
mkZ3Formula atoms p =
    case p of
        T           -> mkTrue
        F           -> mkFalse
        Atom _ i    -> return (atoms ! i)
        Not' p'      -> mkNot =<< (mkZ3Formula atoms p')
        Conj' ps     -> mkAnd =<< (mapM (mkZ3Formula atoms) ps)
        Disj' ps     -> mkOr  =<< (mapM (mkZ3Formula atoms) ps)
        {-
        Impl' p1 p2  -> do
                        p1' <- mkZ3Formula atoms p1
                        p2' <- mkZ3Formula atoms p2
                        mkImplies p1' p2'
        Iff' p1 p2   -> do
                        p1' <- mkZ3Formula atoms p1
                        p2' <- mkZ3Formula atoms p2
                        mkIff p1' p2'-}

mkZ3Script :: Universe -> Prop -> Z3 Result
mkZ3Script u p = do
    atoms <- mapM mkFreshBoolVar u
    assert =<< (mkZ3Formula atoms p)
    check
    
checkSAT :: Prop -> IO Result
checkSAT p = evalZ3 (mkZ3Script (getUniverse p) p)

sat :: Prop -> IO Bool
sat p = (return . ((==) Sat)) =<< checkSAT p

unsat :: Prop -> IO Bool
unsat p = (return . ((==) Unsat)) =<< checkSAT p
