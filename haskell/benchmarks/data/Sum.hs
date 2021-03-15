module Sum where
import Prelude(Int,Bool)
import Debug.Trace

data Maybe a =
    Some a
  | None  

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
