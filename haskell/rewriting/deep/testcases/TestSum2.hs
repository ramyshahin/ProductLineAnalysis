module TestSum2 where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data B = BFalse | BTrue

f :: B -> Int
f b =
    case b of
        BFalse -> 0
        BTrue  -> 1
