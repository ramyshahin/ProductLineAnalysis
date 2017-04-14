-- Propositional Logic and SAT solving
-- Ramy Shahin
-- Jan 3rd 2017
module Prop where

import Z3.Monad
import Data.Vector as V(Vector, (!), fromList, empty, length, snoc, findIndex)
import Data.List
import System.IO.Unsafe

type Universe = Vector String

mkUniverse :: [String] -> Universe
mkUniverse = fromList

queryOrUpdate :: Universe -> String -> (Universe, Int)
queryOrUpdate u q =
    let i = V.findIndex (== q) u
    in  case i of
            Nothing -> (V.snoc u q, V.length u)
            Just i' -> (u, i')
         
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

neg p = simplify (Not' p)

conj  ps = simplify(Conj' ps)

disj ps =  simplify(Disj' ps)

impl p q = simplify(Impl' p q)

iff p q = simplify(Iff' p q)

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
        Impl' p1 p2 -> maxUniverse (getUniverse p1) (getUniverse p2)
        Iff'  p1 p2 -> maxUniverse (getUniverse p1) (getUniverse p2)

mkZ3Formula :: Vector AST -> Prop -> Z3 AST
mkZ3Formula atoms p =
    case p of
        T           -> mkTrue
        F           -> mkFalse
        Atom _ i    -> return (atoms ! i)
        Not' p'      -> mkNot =<< (mkZ3Formula atoms p')
        Conj' ps     -> mkAnd =<< (mapM (mkZ3Formula atoms) ps)
        Disj' ps     -> mkOr  =<< (mapM (mkZ3Formula atoms) ps)
        Impl' p1 p2  -> do
                        p1' <- mkZ3Formula atoms p1
                        p2' <- mkZ3Formula atoms p2
                        mkImplies p1' p2'
        Iff' p1 p2   -> do
                        p1' <- mkZ3Formula atoms p1
                        p2' <- mkZ3Formula atoms p2
                        mkIff p1' p2'

mkZ3Script :: Universe -> Prop -> Z3 Result
mkZ3Script u p = do
    atoms <- mapM mkFreshBoolVar u
    assert =<< (mkZ3Formula atoms p)
    check
    
checkSAT :: Prop -> Result
checkSAT p = unsafePerformIO (evalZ3 (mkZ3Script (getUniverse p) p))

sat :: Prop -> Bool
sat p = (checkSAT p) == Sat

unsat :: Prop -> Bool
unsat p = (checkSAT p) == Unsat

tautology :: Prop -> Bool
tautology p = unsat (neg p)

implies :: Prop -> Prop -> Bool
implies x y = tautology (impl x y)
