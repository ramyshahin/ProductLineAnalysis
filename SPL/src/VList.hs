module VList where

import SPL

data List' a =
    VNull
  | VCons (Var a) (Var (List' a))
  deriving(Show)

type VList a = Var (List' a)

vlength :: VList a -> Var Int
vlength VNull = (mkVarT 0)
vlength (VCons h tl) = (mkVarT 1) |+| (vlength tl)

