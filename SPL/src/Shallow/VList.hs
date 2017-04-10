module Shallow.VList where

import SPL

type VList a = Var [a]

vNil :: Var [a]
vNil = mkVarT []

vCons :: Var a -> Var [a] -> Var [a]
vCons = liftV2 (:)
