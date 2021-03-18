{-# LANGUAGE TypeFamilies #-}
module ListDeep where
import SPL
import VPreludeDeep
import Debug.Trace
import List(List)

type family Lifted t :: *

data List_ a = List_ { f_Nil :: (I_Nil a), f_Cns :: (I_Cns a) }
d_List_ = List_ d_Nil d_Cns

type instance Lifted (List a) = List_ a

data I_Nil a = I_Nil
data I_Cns a = I_Cns (Var a) (List_ a)
d_Nil = I_Nil
d_Cns = I_Cns (Var []) d_List_ --(Var [])
c_Nil = List_ (I_Nil) d_Cns
c_Cns v0 v1 = List_ d_Nil (I_Cns v0 v1)
len :: List_ a -> Int_
len l  = unions [(\(I_Nil) -> (0 ^| ttPC)) (f_Nil (l)), (\(I_Cns x xs) -> ((+) ^| ttPC) <*> ((1 ^| ttPC)) <*> (len (xs))) (f_Cns (l))]

