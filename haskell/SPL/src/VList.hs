{-# LANGUAGE NoImplicitPrelude #-}module VList where
import SPL
import ShallowTypes

data I_List a = Proxy_List (VList a) | I_Nil | I_Cons (V a) (VList a)

type VList a = V (I_List a)
head :: VList a -> (V a)

head xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of Proxy_List p -> head (p /^ __cntxt__)
                                                                                I_Cons x xs -> (x /^ __cntxt__))
tail :: VList a -> VList a

tail xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of Proxy_List p -> tail (p /^ __cntxt__)
                                                                                I_Nil -> (I_Nil ^| __cntxt__)
                                                                                I_Cons x xs -> (xs /^ __cntxt__))
lmap :: ((V a) -> (V b)) -> VList a -> VList b

lmap f xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of Proxy_List p -> lmap (f /^ __cntxt__) (p /^ __cntxt__)
                                                                                  I_Nil -> (I_Nil ^| __cntxt__)
                                                                                  I_Cons y ys -> (I_Cons (f (y /^ __cntxt__)) (lmap (f /^ __cntxt__) (ys /^ __cntxt__)) ^| __cntxt__))
lfoldr :: ((V a) -> (V b) -> (V b)) -> (V b) -> VList a -> (V b)

lfoldr f i xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of Proxy_List p -> lfoldr (f /^ __cntxt__) (i /^ __cntxt__) (p /^ __cntxt__)
                                                                                      I_Nil -> (i /^ __cntxt__)
                                                                                      I_Cons y ys -> f (y /^ __cntxt__) (lfoldr (f /^ __cntxt__) (i /^ __cntxt__) (ys /^ __cntxt__)))
lfoldl :: ((V b) -> (V a) -> (V b)) -> (V b) -> VList a -> (V b)

lfoldl f i xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of Proxy_List p -> lfoldl (f /^ __cntxt__) (i /^ __cntxt__) (p /^ __cntxt__)
                                                                                      I_Nil -> (i /^ __cntxt__)
                                                                                      I_Cons y ys -> lfoldl (f /^ __cntxt__) (f (i /^ __cntxt__) (y /^ __cntxt__)) (ys /^ __cntxt__))
len :: VList a -> VInt
len xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of Proxy_List p -> len (p /^ __cntxt__)
                                                                               I_Nil -> (0 ^| __cntxt__)
                                                                               I_Cons x xs' -> toSubV ((1 ^| __cntxt__) + len (xs' /^ __cntxt__)))

