module SumDeep where
import SPL
import VPreludeDeep
import Prelude(Int(..),Bool(..))
import Debug.Trace

data Maybe_ a = Maybe_ { f_Some :: (I_Some a), f_None :: (I_None a) }  

data I_Some a = I_Some (Var a)
data I_None a = I_None
d_Some = I_Some (Var [])
d_None = I_None
c_Some v0 = Maybe_ (I_Some v0) d_None
c_None = Maybe_ d_Some (I_None)
isSome :: Maybe_ a -> Bool_
isSome x  = unions [(\(I_Some y) -> (True ^| ttPC)) (f_Some (x)), (\(I_None) -> (False ^| ttPC)) (f_None (x))]

--isNone :: Maybe a -> Bool
--isNone x = 
--    case x of
--        None -> True
--        _    -> False  

