module TestSum5 where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data Option a = None | Some a

plus :: Option Int -> Option Int -> Option Int
plus a b =
    case a of
        None -> None
        Some x -> case b of 
                     None -> None
                     Some y -> Some (x + y)
