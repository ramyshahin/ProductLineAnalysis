{-# LANGUAGE NoImplicitPrelude #-}module VTestProd1 where
import SPL
import VPrelude
import Data.List -- TODO: without a dummy import, indentation goes wrong

data I_T = I_T VInt VInt

type VT = V I_T
fstT :: VT -> VInt

fstT t  = match t (\(t, pc) -> let __cntxt__ = __cntxt__ /\ pc in case t of I_T f s -> (f /^ __cntxt__))
sndT :: VT -> VInt
sndT t  = match t (\(t, pc) -> let __cntxt__ = __cntxt__ /\ pc in case t of I_T f s -> (s /^ __cntxt__))

