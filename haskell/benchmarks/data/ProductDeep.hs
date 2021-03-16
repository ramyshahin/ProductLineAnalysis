module ProductDeep where
import SPL
import VPreludeDeep
import Debug.Trace

data Point_ = Point_ { f_Point :: I_Point }
data I_Point = I_Point Int_ Int_
d_Point = I_Point (Var []) (Var [])
c_Point v0 v1 = Point_ (I_Point v0 v1)


