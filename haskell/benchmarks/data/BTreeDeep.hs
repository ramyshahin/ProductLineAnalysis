module BTreeDeep where
import SPL
import VPreludeDeep
import Debug.Trace

data BTree_ a = BTree_ { f_Leaf :: (I_Leaf a), f_Node :: (I_Node a) }

d_BTree_ = BTree_ d_Leaf d_Node
data I_Leaf a = I_Leaf
data I_Node a = I_Node (Var a) (BTree_ a) (BTree_ a)
d_Leaf = I_Leaf
d_Node = I_Node d_Var d_BTree_ d_BTree_
c_Leaf = BTree_ (I_Leaf) d_Node
c_Node v0 v1 v2 = BTree_ d_Leaf (I_Node v0 v1 v2)
nodeCount :: BTree_ a -> Int_
nodeCount t  = unions [(\(I_Leaf) -> (0 ^| ttPC)) (f_Leaf (t)), (\(I_Node x r l) -> ((+) ^| ttPC) <*> (((+) ^| ttPC) <*> ((1 ^| ttPC)) <*> (nodeCount r)) <*> (nodeCount l)) (f_Node (t))]

