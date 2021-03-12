module ProductDeep where
import SPL

data Point_ = Point (Var Int) (Var Int)
type Point = Var Point_
_Point = mkVarT . Point


