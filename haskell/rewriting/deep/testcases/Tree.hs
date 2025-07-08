module Tree where

import List

data Tree a = TCons a (List (Tree a))

tlen :: Tree a -> Int
tlen t = 
    case t of
        TCons x xs -> lfoldr (+) 1 (lmap tlen xs) 