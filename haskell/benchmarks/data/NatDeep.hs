module NatDeep where
import SPL
import VPreludeDeep
import Debug.Trace

data Nat = Z | S Nat
type Nat_ = Var Nat
_Z = mkVarT $ Z
_S = mkVarT $ S


