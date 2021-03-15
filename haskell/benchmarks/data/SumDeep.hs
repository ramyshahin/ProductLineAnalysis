module SumDeep where
import SPL
import VPreludeDeep
import Prelude(Int,Bool)
import Debug.Trace

data Maybe a = Maybe_ (C_Some a) (C_None a)
type (Maybe_ a) = Var (Maybe a)
data C_Some a = C_Some (Var a)
data C_None a = C_None  

--isSome :: Maybe a -> Bool
--isSome x = 
--    case x of
--        Some _ -> True
--        _      -> False

--isNone :: Maybe a -> Bool
--isNone x = 
--    case x of
--        None -> True
--        _    -> False  

