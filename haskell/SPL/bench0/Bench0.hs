-- SPL quickcheck properties
module Main where

import SPL
import PropBDD
import Control.Applicative

p, q, r, s :: Prop
univ@[p, q, r, s] = mkUniverse ["P", "Q", "R", "S"]
--p = lookup u 0
--q = lookup u 1
--r = lookup u 2
--s = lookup u 3

pq = conj[p,q]
p_q = conj[p, neg q]
_pq = conj[neg p, q]
_p_q = conj[neg p, neg q]
_p = neg p
_q = neg q 
{-
v1, v2 :: Var Int
v1 = mkVars [(1,pq), (2,p_q), (1, _pq), (2, _p_q)]
v2 = mkVars [(1,q), (2, _q)]

w :: Var Int
w = mkVars [(12, pq), (2, p_q), (3, _p_q)]
-}
x :: Var Int
x = mkVars [(7, pq), (-3, p_q), (-8, _pq), (0, _p_q)]
{-
x0 = mkVars [(-8, _pq), (0, _p_q), (-3, p_q), (7, pq)]
x1 = mkVarT 7

y :: Var Int
y = mkVars [(-11, _p)]
y0 = mkVars [(-11, _pq)]

z :: Var Int
z = mkVars [(6, p)]

xs = [1..5]
-}

--foo :: Int -> Int
--{-# NOINLINE foo #-}
--foo x = x + 2

--bar :: Int -> Int
--{-# NOINLINE bar #-}
--bar x = x * 3

{-# NOINLINE plus #-}
plus = (+)

--baz :: Int -> Int -> Int
--{-# INLINE [1] baz #-}
--baz x y = plus (foo x) (bar y)

--baz' :: Var (Int -> Int -> Int)
--{-# INLINE baz' #-}
--baz' = mkVarT baz

--bazDeep0 :: Var Int -> Var Int -> Var Int
--bazDeep0 xs ys = apply (apply (mkVarT plus) (apply (mkVarT foo) xs)) (apply (mkVarT bar) ys)

--plusDeep = liftV2 (+)
--fooDeep  xs = (liftV2 (+)) xs (mkVarT 2)
--barDeep  xs = (liftV2 (*)) xs (mkVarT 3)

--bazDeep1 :: Var Int -> Var Int -> Var Int
--bazDeep1 xs ys = plusDeep (fooDeep xs) (barDeep ys)

--bar' = mkVarT bar
--foo' = mkVarT foo
--plus' = mkVarT (+)

--baz'' = (\x y -> x + y)

--{-# NOINLINE bla #-}
--bla x' = let (Var x) = compact x'
--         in mkVars [(baz'' x'', tt) | (x'',xpc) <- x, let pc = conj[tt,xpc], sat(pc)]

foo0 :: Int -> Int
{-# INLINE [1] foo0 #-}
foo0 x = plus x 1

--boom x = x * 0

{-# INLINE [1] foo1 #-}
foo1 x = foo0 x

{-# RULES
  --"deep/lift0" forall (f :: Int -> Int -> Int). apply (mkVarT f) = apply (Var[(baz'', tt)])
  --"deep/lift1" forall (f :: Int -> Int -> Int) pc. apply_ (f, pc) = bla
  --"deep/lift2" forall pc. apply_ (\x y -> plus (foo x) (bar y), pc) = bla
  --"deep/lift3" forall (f :: Int -> Int -> Int) (g :: Int -> Int) (h :: Int -> Int) pc. apply_ (\x y -> f (g x) (h y), pc) = bla
  --"deep/lift4" forall (f :: a -> a -> a) (m :: a -> a) (n :: a -> a) pc. 
  --                apply_ (\x y -> f (m x) (n y), pc) = apply_ (f, pc) 
  "deep/lift5" [2] forall xs pc. 
                  apply_ (\x -> foo1 x, pc) xs = xs
  #-}
--plus' <*> (foo' <*> a) <*> (bar' <*> b)

main = do 
    --print $ apply (apply (mkVarT baz) x) x1
    print $ apply (mkVarT foo1) x