-- Propositional Logic and SAT solving
-- Ramy Shahin
-- Jan 3rd 2017
module Prop where

import Z3.Monad
import Data.Vector(Vector, (!), fromList)

type Universe = Vector String

mkUniverse :: [String] -> Universe
mkUniverse = fromList

data Prop =
    T
  | F
  | Atom Universe Int
  | Not Prop
  | Conj [Prop]
  | Disj [Prop]
  | Impl Prop Prop
  | Iff  Prop Prop

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
            Not p       -> "Not(" ++ show p ++ ")"
            Conj ps     -> "And(" ++ showPropList ps ++ ")"
            Disj ps     -> "Or("  ++ showPropList ps ++ ")"
            Impl p1 p2  -> "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
            Iff  p1 p2  -> "(" ++ show p1 ++ " <=> " ++ show p2 ++ ")"

mkZ3Formula :: Vector AST -> Prop -> Z3 AST
mkZ3Formula atoms p =
    case p of
        T           -> mkTrue
        F           -> mkFalse
        Atom _ i    -> return (atoms ! i)
        Not p'      -> mkNot =<< (mkZ3Formula atoms p')
        Conj ps     -> mkAnd =<< (mapM (mkZ3Formula atoms) ps)
        Disj ps     -> mkOr  =<< (mapM (mkZ3Formula atoms) ps)
        Impl p1 p2  -> do
                        p1' <- mkZ3Formula atoms p1
                        p2' <- mkZ3Formula atoms p2
                        mkImplies p1' p2'
        Iff p1 p2   -> do
                        p1' <- mkZ3Formula atoms p1
                        p2' <- mkZ3Formula atoms p2
                        mkIff p1' p2'

mkZ3Script :: Universe -> Prop -> Z3 Result
mkZ3Script u p = do
    atoms <- mapM mkFreshBoolVar u
    assert =<< (mkZ3Formula atoms p)
    check
    
checkSAT :: Universe -> Prop -> IO Result
checkSAT u p = evalZ3 (mkZ3Script u p)

sat :: Universe -> Prop -> IO Bool
sat u p = (return . ((==) Sat)) =<< checkSAT u p

unsat :: Universe -> Prop -> IO Bool
unsat u p = (return . ((==) Unsat)) =<< checkSAT u p
