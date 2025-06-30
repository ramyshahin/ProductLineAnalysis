{-# LANGUAGE NoImplicitPrelude #-}{-# LANGUAGE NoImplicitPrelude #-}

module BoolDeep where
import SPL
import VPrelude
import Data.List -- TODO: without a dummy import, indentation goes wrong

data I_Bool = I_False | I_True

type VBool = V I_Bool
not :: VBool -> VBool

not x  = match x (\(x, pc) -> let __cntxt__ = __cntxt__ /\ pc in case x of I_False -> (I_True ^| __cntxt__)
                                                                           I_True -> (I_False ^| __cntxt__))
ite :: VBool -> (V a) -> (V a) -> (V a)

ite c x y  = match c (\(c, pc) -> let __cntxt__ = __cntxt__ /\ pc in case c of I_True -> (x /^ __cntxt__)
                                                                               I_False -> (y /^ __cntxt__))
and :: VBool -> VBool -> VBool

and x y  = ite (x) (y) ((v I_False)) 
or :: VBool -> VBool -> VBool 
or x y  = ite (x) ((v I_True)) (y)

