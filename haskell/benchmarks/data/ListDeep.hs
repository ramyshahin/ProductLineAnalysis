module ListDeep where
import SPL
import VPreludeDeep
import Debug.Trace

data List a = List_ { fil :: (Nil a), fns :: (Cns a) }

List = List Nil Cns
data Nil a = Nil
data Cns a = Cns (Var a) (List a)
Nil = Nil
Cns = Cns Var List
Nil = List (Nil) Cns
Cns v0 v1 = List Nil (Cns v0 v1)
len :: List a -> Int
len l  = unions [(\(Nil) -> (0 ^| ttPC)) (Nil (l)), (\(Cns x xs) -> ((+) ^| ttPC) <*> ((1 ^| ttPC)) <*> (len xs)) (Cns (l))]

