module TestSum4 where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data IntOption = None | Some Int

plus :: IntOption -> IntOption -> IntOption
plus a b =
    case a of
        None -> None
        Some x -> case b of 
                     None -> None
                     Some y -> Some (x + y)
