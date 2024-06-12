module TestSum3 where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data B = BFalse | BTrue

f :: B -> B
f b =
    case b of
        BFalse -> BTrue
        BTrue  -> BFalse
