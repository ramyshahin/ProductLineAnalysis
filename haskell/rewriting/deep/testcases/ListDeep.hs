{-# LANGUAGE NoImplicitPrelude #-}module ListDeep where
import SPL
import VPrelude
import Data.Maybe

data I_List a = Proxy_List (VList a) | I_Nil | I_Cons (V a) (VList a)

type VList a = V (I_List a)
head :: VList a -> (V a)

head xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of I_Cons x xs -> (x /^ __cntxt__))
tail :: VList a -> VList a

tail xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of I_Nil -> (I_Nil ^| __cntxt__)
                                                                                I_Cons x xs -> (xs /^ __cntxt__))
len :: VList a -> VInt
len xs  = match xs (\(xs, pc) -> let __cntxt__ = __cntxt__ /\ pc in case xs of I_Nil -> (0 ^| __cntxt__)
                                                                               I_Cons x xs' -> toSubV ((1 ^| __cntxt__) + len (xs' /^ __cntxt__)))

