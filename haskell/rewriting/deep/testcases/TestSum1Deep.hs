{-# LANGUAGE NoImplicitPrelude #-}module TestSum1Deep where
import SPL
import VPrelude
import Data.List -- TODO: without a dummy import, indentation goes wrong

data I_False = I_False
data I_True = I_True
data VBool = VBool_SOP { f_False :: SumOption I_False, f_True :: SumOption I_True }

