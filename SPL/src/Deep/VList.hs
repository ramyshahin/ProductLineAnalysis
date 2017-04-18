module Deep.VList where

import SPL
import Prop -- for testing

type VList a = Var [Var a]

vCons :: Var a -> VList a -> VList a 
vCons x@(Var x') xs@(Var xs') = union def undef 
  where def = Var [(r : xs'', (definedAt r)) | (xs'', xspc) <- xs', let r@(Var r') = restrict xspc x, not (null r')]
        undef = restrict (undefinedAt x) xs

vNil = mkVarT []

mkVList :: [Var a] -> VList a
mkVList xs = foldr vCons vNil xs

vhead :: VList a -> Var a 
vhead xs = union2 $ (liftV head) xs

vtail :: VList a -> VList a
vtail xs = (liftV tail) xs

vlength :: VList a -> Var Int
vlength = (liftV length)
          
vmap :: Var (a -> b) -> VList a -> VList b
vmap f xs@(Var xs') = Var [(map (\x -> f <*> x) xs'', pc) | (xs'',pc) <- xs']

