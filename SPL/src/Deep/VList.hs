module Deep.VList where

import SPL

data VList a =
    VE 
  | a |:| Var (VList a)