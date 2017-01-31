-------------------------------------------------------------------------------
-- SPL.hs
-- Software Product Line library
-- Ramy Shahin - July 14th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module SPL where

import Prop
import Control.Applicative
import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.List 
import Data.Maybe
--import Debug.Trace
import Control.Exception

type FeatureSet         = Universe
type PresenceCondition  = Prop

type Val a = (Maybe a, PresenceCondition)

--instance Eq SPLOption a where
--    (==) a b = (getValue a == getValue b) && sat(getPresenceCondition a && getPresenceCondition b)

-- when lifting a product value to a product line value, we might end up with
-- different values for each product in the product line. This is why a value is
-- lifted into a set of values, each with a path condition. An important
-- inVar here is that the path conditions of those values should not depend
-- on each other, i.e. non of them logically implies the other. Violating this
-- inVar would result in redundant values (i.e. multiple values belonging
-- to the same set of products). This does not affect correctness, but severely
-- affects performance as we are now degenerating into brute force analysis
-- across all possible products.
data Var t = Var [(Val t)]

--type family Var t where
--    Var ((t :: * -> * -> * -> *) (s1 :: *) (s2 :: *) (s3 :: *))      = Var' (t (Var' s1) (Var' s2) (Var' s3))
--    Var ((t :: * -> * -> *) (s1 :: *) (s2 :: *))      = Var' (t (Var' s1) (Var' s2))
--    Var ((t :: * -> *) (s :: *))      = Var' (t (Var' s))
--    Var (t :: *)                      = Var' t

mkVar :: t -> PresenceCondition -> Var t
mkVar v pc = Var [(Just v,pc), (Nothing, neg pc)]

mkVarT :: t -> Var t
mkVarT v = mkVar v T

mkVars :: [(t,PresenceCondition)] -> Var t
mkVars vs = let nothingPC = (neg . disj) (map snd vs) 
                vs'       = map (\(v,pc) -> (Just v, pc)) vs
            in  Var ((Nothing, nothingPC) : vs')


--mkVars vs = Var (map (\(v,pc) -> (Just v, pc)) vs)

-- compaction seems to be turning some lazy expressions into strict,
-- resulting in condiitional expression bugs

compact :: (Eq t) => Var t -> Var t
compact (Var v) = 
    let gs = groupBy (\(v1, _) (v2, _) -> (v1 == v2)) v
    in  Var (map (\g -> let (vs, pcs) = unzip g
                        in  (head vs, disj pcs)) 
            gs)

valIndex :: Var t -> PresenceCondition -> [Val t]
valIndex (Var v) pc =
    filter (\(_,pc') -> sat (conj[pc, pc'])) v

subst :: Show t => Var t -> PresenceCondition -> Var t
subst (Var v) pc =
    Var (filter (\(v',pc') -> sat (conj [pc,pc'])) v)

union :: Var t -> Var t -> Var t
union (Var a) (Var b) =
    Var (a ++ b)

pairs :: [t] -> [(t,t)]
pairs [] = []
pairs xs = zip xs (tail xs)

inv :: Show t => Var t -> Bool
inv (Var v) = {-trace ("inv: " ++ (show (Var v))) $-} 
    all (\((_, pc1),(_, pc2)) -> unsat (conj[pc1, pc2])) (pairs v)

apply :: Var (a -> b) -> Var a -> Var b

apply (Var fn) (Var x) =
    Var [(case (fn', x') of
        (Just fn'', Just x'') -> Just (fn'' x'')
        (_,_) -> Nothing
        , pc) 
                  | (fn', fnpc) <- fn,
                    (x', xpc) <- x,
                    let pc = conj[fnpc, xpc],
                    sat pc]
{-
apply (Var fn) (Var x) = 
    let triples = [(fn', x', c) |   (fn', fnpc) <- fn,
                                    (x', xpc) <- x,
                                    let c= conj[fnpc, xpc],
                                    sat c                   ]
    in  Var (map (\(fn', x', c) ->  (case (fn', x') of
                                        (Just fn'', Just x'') -> fn'' `seq` Just (fn'' x'')
                                        (_,_) -> Nothing
    , c)) triples)
  -}      

instance Show a => Show (Var a) where
    show (Var v) = "{\n" ++ (foldr (++) "" (map (\x -> (show x) ++ "\n") v)) ++ "}" 

instance Functor Var where
    fmap :: (a -> b) -> Var a -> Var b
    fmap f = apply (mkVarT f)

instance Applicative Var where
    pure  = mkVarT
    (<*>) = apply

-- lifting conditional expression
cond :: Bool -> a -> a -> a
cond p a b = if p then a else b

cond' :: Show a => Var Bool -> Var a -> Var a -> Var a

{-
cond' (Var c) a b = 
    let ts = filter (\(p, _) -> p == Just True) c
        fs = filter (\(p, _) -> p == Just False) c
        --ns = filter (\(p, _) -> p == Nothing) c
        tEls = foldr (++) [] $ map (\(_,pc) -> narrow a pc) ts
        fEls = foldr (++) [] $ map (\(_,pc) -> narrow b pc) fs
        --nEls = [(Nothing, pc) | (_,pc) <- ns]
        res = tEls ++ fEls -- ++ nEls
    in  trace ("cond': " ++ (show ts) ++ "\t" ++ (show fs)) (Var res)

cond' (Var c) (Var a) (Var b) =
    Var [(Just (if c' then a' else b'), pc)    | (Just c', cpc) <- c,
                                          (Just a', apc) <- a,
                                          (Just b', bpc) <- b,
                                          let pc = (conj [cpc, apc, bpc]),
                                          sat pc]
-}
cond' = liftV3 cond

-- lifting higher-order functions
mapLifted :: Var (a -> b) -> Var [a] -> Var [b]
mapLifted = liftA2 map

filterLifted :: Var (a -> Bool) -> Var [a] -> Var [a]
filterLifted = liftA2 filter

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = fmap f a <*> b <*> c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = fmap f a <*> b <*> c <*> d <*> e

liftV :: (a -> b) -> Var a -> Var b
liftV = liftA

liftV2 :: (a -> b -> c) -> Var a -> Var b -> Var c
liftV2 = liftA2

liftV3 :: (a -> b -> c -> d) -> Var a -> Var b -> Var c -> Var d
liftV3 = liftA3

liftV4 :: (a -> b -> c -> d -> e) -> Var a -> Var b -> Var c -> Var d -> Var e
liftV4 = liftA4

liftV5 :: (a -> b -> c -> d -> e -> f) -> Var a -> Var b -> Var c -> Var d -> Var e -> Var f
liftV5 = liftA5

cliftV  f a = compact $ (liftV f) a
cliftV2 f a b = compact $ (liftV2 f) a b
cliftV3 f a b c = compact $ (liftV3 f) a b c
cliftV4 f a b c d = compact $ (liftV4 f) a b c d
cliftV5 f a b c d e = compact $ (liftV5 f) a b c d e

--data VarOption a =
    
-- Bool operation lifting
(|==|) :: (Eq a) => Var a -> Var a -> Var Bool
(|==|) = cliftV2 (==)

{-
-- List lifting
data List' t =
    Empty'
  | Cons' t (Var' (List' t))
-}
e :: Var [a]
e = mkVarT []

(|:|) :: (Show a) => Var a -> Var [a] -> Var [a]
(|:|) (Var v) (Var vs) = 
    let ts = [(v', vs', c) |    (v', vpc) <- v, 
                                (vs', vspc) <- vs, 
                                let c = conj[vpc, vspc], 
                                sat c]
        res = map (\(v', vs', pc) -> (case (v', vs') of
                                        (Just v'', Just vs'') -> Just (v'' : vs'')
                                        (Nothing, Just vs'')  -> Just vs''
                                        (_, _)                -> Nothing
                                    , pc)
                ) ts
    in Var res

--(|:|) = liftV2 (:)

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f e xs =
    if (null xs)
    then e
    else f (head xs) (foldr_ f e (tail xs))

foldr' :: (Show a, Show b) => Var (a -> b -> b) -> Var b -> Var [a] -> Var b
--foldr' = liftV3 foldr
foldr' f' e' xs' =
    cond'   (null' xs')
            e'
            (f' <*> (head' xs') <*> (foldr' f' e' (tail' xs')))

mkVarList :: (Show t) => [Var t] -> Var [t]
mkVarList = foldr (|:|) e

mkVarList' :: (Show t) => Var [t] -> Var [t]
mkVarList' = foldr' (pure (:)) e

null' :: (Foldable t, Show (t a)) => Var (t a) -> Var Bool
null' xs = let res = ((cliftV null) xs)
            in {-trace ("null': " ++ (show res))-} res

head' :: Var [a] -> Var a
head' = liftV head

tail' :: Var [a] -> Var [a]
tail' = liftV tail
