module TestRecursive1 where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data LList a = NNil | CCons a (LList a)

llength :: LList a -> Int
llength l =
    case l of
        NNil -> 0
        CCons x xs -> 1 + llength xs

