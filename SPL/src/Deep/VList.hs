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
  where def = Var [(VCons' hd (Var[p]), (definedAt hd)) | p@(xs'',xspc) <- xs', let hd = (restrict xspc x)]
        undef = restrict (undefinedAt x) xs

mkVList :: [Var a] -> VList a
mkVList xs = foldr vCons vNil xs

head' :: VList' a -> Var a 
head' xs = case xs of
              VCons' x xs'' -> x 
              _ -> error ""

vhead :: VList a -> Var a 
vhead xs = union2 $ (liftV head') xs

tail' :: VList' a -> VList a
tail' xs = case xs of
            VNil' -> error ""
            VCons' x xs -> xs

vtail :: VList a -> VList a
vtail xs = union2 $ (liftV tail') xs

length' :: VList' t -> Var Int
length' VNil' = mkVarT 0
length' (VCons' x xs) = (mkVar 1 (definedAt x)) |+| (vlength xs)

vlength xs = union2 $ (liftV length') xs

map' :: (a -> b) -> VList' a -> VList' b
map' f xs = case xs of
              VNil' -> VNil'
              VCons' x xs' -> VCons' ((liftV f) x) (vmap (mkVarT f) xs')
          
vmap :: Var (a -> b) -> VList a -> VList b
vmap = liftV2 map'

