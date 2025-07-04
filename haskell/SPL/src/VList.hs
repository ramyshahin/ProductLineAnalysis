{-# LANGUAGE NoImplicitPrelude, CPP #-}
module VList where
#ifdef COMPACTION
import SPLOpt
#else
import SPL 
#endif
import VPrelude

{-
data I_Nil a = I_Nil

data I_Cons a = I_Cons a ((VList a))
data VList a = VList_PoS { f_VProxyVList :: SumOption (I_VProxy (VList a)), f_Nil :: SumOption (I_Nil a), f_Cons :: SumOption (I_Cons a) }

instance  VClass a =>VClass ((VList a)) where nil = VList_PoS nil nil nil
                                              comb a b = VList_PoS (comb (f_VProxyVList a) (f_VProxyVList b)) (comb (f_Nil a) (f_Nil b)) (comb (f_Cons a) (f_Cons b))
                                              proxy = resolveVProxy . f_VProxyVList
instance  VClass a =>VClass (I_Nil a) where nil = I_Nil
                                            comb I_Nil I_Nil = I_Nil

instance  VClass a =>VClass (I_Cons a) where nil = I_Cons nil nil
                                             comb (I_Cons a1 a2) (I_Cons b1 b2) = I_Cons ((comb a1 b1)) ((comb a2 b2))
consI_Nil x r = r { f_Nil = x }
consI_Cons x r = r { f_Cons = x }
head :: VClass a => (VList a) -> a
head xs  = match [(\(Present (I_Cons x xs, pc)) r -> x) (f_Cons (xs))]
tail :: VClass a => (VList a) -> (VList a)
tail xs  = match [(\(Present (I_Nil, pc)) r -> (consI_Nil (Present (I_Nil, allConfigs)) nil)) (f_Nil (xs)), (\(Present (I_Cons x xs, pc)) r -> xs) (f_Cons (xs))]
len :: VClass a => (VList a) -> VInt
len xs  = match [(\(Present (I_Nil, pc)) r -> ((v 0))) (f_Nil (xs)), (\(Present (I_Cons x xs', pc)) r -> ((v 1)) + len xs') (f_Cons (xs))]
-}
