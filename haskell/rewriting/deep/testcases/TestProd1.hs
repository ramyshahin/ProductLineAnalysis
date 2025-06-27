module TestProd1 where
import Data.List -- TODO: without a dummy import, indentation goes wrong

data T = T Int Int

fstT :: T -> Int
fstT t =
    case t of
        T f s -> f

sndT :: T -> Int
sndT t =
    case t of
        T f s -> s
