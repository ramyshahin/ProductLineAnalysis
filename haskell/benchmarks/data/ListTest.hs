module ListTest where
import ListDeep
import SPL

l0 :: List_ Int
l0 = _Nil

l1 :: list_ Int
l1 = _Cns (mkVarT 0) <*> l0

