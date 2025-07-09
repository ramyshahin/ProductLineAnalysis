module Tree where

import List

data Tree a = Empty | Tree a (Tree a) (Tree a) (Tree a) (Tree a) (Tree a)

tlen :: Tree a -> Int
tlen t = 
    case t of
        Empty -> 0
        Tree x t1 t2 t3 t4 t5 -> 1 + tlen t1 + tlen t2 + tlen t3 + tlen t4 + tlen t5 
