module Deep.VList where

import SPL
import Prop -- for testing

data List a =
    NIL 
  | Cons a (List a)

data VList' a =
    VNil'
  | VCons' (Var a) (VList a)
  deriving Show
  
type VList a = Var (VList' a)

vNil = mkVarT VNil'

vCons :: Var a -> VList a -> VList a 
vCons x@(Var x') xs@(Var xs') = union def undef 
  where xdef = defSubst x
        def = Var [(Just (VCons' hd (Var[p])), (definedAt hd)) | p@(xs'',xspc) <- xs', let hd = (restrict xspc xdef)]
        undef = restrict (undefinedAt x) xs

mkVList :: [Var a] -> VList a
mkVList xs = foldr vCons vNil xs

head' :: VList' a -> Var a 
head' xs = case xs of
              VCons' x xs'' -> x 
              _ -> error ""

vHead :: VList a -> Var a 
vHead xs = union2 $ (liftV head') xs

tail' :: VList' a -> VList a
tail' xs = case xs of
            VNil' -> error ""
            VCons' x xs -> xs

vTail :: VList a -> VList a
vTail xs = union2 $ (liftV tail') xs

