module BTree where
import Debug.Trace

data BTree a =
    Leaf
  | Node a (BTree a) (BTree a)

nodeCount :: BTree a -> Int
nodeCount t =
    case t of
        Leaf -> 0
        Node x r l -> 1 + nodeCount r + nodeCount l
