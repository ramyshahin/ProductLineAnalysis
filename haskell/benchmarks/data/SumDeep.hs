module SumDeep where
import SPL
import Debug.Trace

data Maybe_ a = Some (Var a) | None
type (Maybe a) = Var (Maybe_ a)
_Some = mkVarT $ Some
_None = mkVarT $ None    

