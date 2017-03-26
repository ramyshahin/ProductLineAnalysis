module Shallow.VList where

import SPL

type VList a = Var [a]

ve :: Var [a]
ve = mkVarT []

vcons :: Var a -> Var [a] -> Var [a]
vcons = liftV2 (:)
