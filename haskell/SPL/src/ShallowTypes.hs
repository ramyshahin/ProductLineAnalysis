-- shallow lifting of primitive types
{-# LANGUAGE NoImplicitPrelude, CPP #-}

module ShallowTypes where
import qualified Prelude as P
-- #ifdef COMPACTION
-- import SPLOpt
-- #else
import SPL 
-- #endif

type Int = P.Int
type Bool = P.Bool
type VInt = V Int

class P.Num a => VNum a where
    (+) :: V a -> V a -> V a 
    x + y = (P.pure (P.+)) P.<*> x P.<*> y

    (*) :: V a -> V a -> V a 
    x * y = (P.pure (P.*)) P.<*> x P.<*> y

    (-) :: V a -> V a -> V a 
    x - y = (P.pure (P.-)) P.<*> x P.<*> y  

    negate :: V a -> V a
    negate x = (P.pure P.negate) P.<*> x

    abs :: V a -> V a
    abs x = (P.pure P.abs) P.<*> x

    signum :: V a -> V a
    signum x = (P.pure P.signum) P.<*> x

    fromInteger :: V P.Integer -> V a 
    fromInteger x = (P.pure P.fromInteger) P.<*> x

instance VNum P.Int
