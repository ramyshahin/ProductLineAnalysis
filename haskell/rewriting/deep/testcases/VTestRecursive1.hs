{-# LANGUAGE NoImplicitPrelude #-}module VTestRecursive1 where
import SPL
import VPrelude
import Data.List -- TODO: without a dummy import, indentation goes wrong

data I_LList a = Proxy_LList (VLList a) | I_NNil | I_CCons (V a) (VLList a)

type VLList a = V (I_LList a)
llength :: VLList a -> VInt
llength l  = match l (\(l, pc) -> let __cntxt__ = __cntxt__ /\ pc in case l of I_NNil -> (0 ^| __cntxt__)
                                                                               I_CCons x xs -> toSubV ((1 ^| __cntxt__) + llength (xs /^ __cntxt__)))


