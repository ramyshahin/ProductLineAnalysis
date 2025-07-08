{-# LANGUAGE NoImplicitPrelude #-}module VTestSum4 where
import SPL
import VPrelude
import Data.List -- TODO: without a dummy import, indentation goes wrong

data I_IntOption = I_None | I_Some VInt

type VIntOption = V I_IntOption
plus :: VIntOption -> VIntOption -> VIntOption
plus a b  = match a (\(a, pc) -> let __cntxt__ = __cntxt__ /\ pc in case a of I_None -> (I_None ^| __cntxt__)
                                                                              I_Some x -> toSubV (match b (\(b, pc) -> let __cntxt__ = __cntxt__ /\ pc in case b of I_None -> (I_None ^| __cntxt__)
                                                                                                                                                                    I_Some y -> (I_Some (toSubV ((x /^ __cntxt__) + (y /^ __cntxt__))) ^| __cntxt__))))

