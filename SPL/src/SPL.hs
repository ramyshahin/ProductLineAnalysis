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
import Data.List 
import Data.Maybe
import Control.Exception
import Control.Parallel.Strategies
import System.Mem.StableName
import System.IO.Unsafe

(===) :: a -> a -> Bool
(!x) === (!y) = unsafePerformIO $ do 
    nx <- makeStableName $! x 
    ny <- makeStableName $! y 
    return (nx == ny)

(====) :: Eq a => a -> a -> Bool
(====) x y = x === y || x == y

--type FeatureSet         = Universe
type PresenceCondition  = Prop

--type Val a = (Maybe a, PresenceCondition)
type Val a = (a, PresenceCondition)

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
newtype Var t = Var [(Val t)]

exists :: Eq t => Val t -> Var t -> Bool
exists (x, xpc) ys' =
    or [(x == y) && (implies xpc ypc) | (y,ypc) <- ys]
    where (Var ys) = compactEq ys'

isSubsetOf :: Eq t => Var t -> Var t -> Bool
isSubsetOf (Var x) y' = and (map (`exists` y') x)

instance Show a => Show (Var a) where
    show v' = 
        let (Var v) = compact v' 
        in "{\n" ++ (foldr (++) "" (map (\x -> (show x) ++ "\n") v)) ++ "}" 

-- a < b means that a is a subset of b in terms of products
instance Eq a => Ord (Var a) where
    (<) x' y' = (isSubsetOf x' y') && not (isSubsetOf y' x')
    (<=) x y = (x < y) || (x == y)

instance Eq a => Eq (Var a) where
    (==) x y = (isSubsetOf x y) && (isSubsetOf y x)
    (/=) x y = not (x == y)

instance Functor Var where
    fmap :: (a -> b) -> Var a -> Var b
    fmap f = apply (mkVarT f)

instance Applicative Var where
    pure  = mkVarT
    (<*>) = apply

--type family Var t where
--    Var ((t :: * -> * -> * -> *) (s1 :: *) (s2 :: *) (s3 :: *))      = Var' (t (Var' s1) (Var' s2) (Var' s3))
--    Var ((t :: * -> * -> *) (s1 :: *) (s2 :: *))      = Var' (t (Var' s1) (Var' s2))
--    Var ((t :: * -> *) (s :: *))      = Var' (t (Var' s))
--    Var (t :: *)                      = Var' t

mkVar :: t -> PresenceCondition -> Var t
mkVar v pc = Var [(v,pc)]

mkVarT :: t -> Var t
mkVarT v = mkVar v tt

mkVars :: [(t,PresenceCondition)] -> Var t
mkVars vs = Var vs

findVal :: t -> [Val t] -> (t -> t -> Bool) -> [Val t]
findVal _ [] _ = []
findVal v ((x,pc):xs) cmp = if (cmp v x) then (x,pc) : rest else rest
    where rest = findVal v xs cmp

phelem :: t -> [t] -> (t -> t -> Bool) -> Bool
phelem v xs cmp = any (\x -> cmp v x) xs

groupVals_ :: [Val t] -> [t] -> (t -> t -> Bool) -> [Val t]
groupVals_ [] _ _ = []
groupVals_ ((x,xpc):xs) ds cmp = 
    if phelem x ds cmp then rest else 
        let ms = findVal x xs cmp
            pc = disj(xpc:(snd . unzip) ms)
        in  (x,pc) : rest
    where rest = groupVals_ xs (x:ds) cmp

groupVals :: [Val t] -> (t -> t -> Bool) -> [Val t]
groupVals xs cmp = groupVals_ xs [] cmp

-- compaction seems to be turning some lazy expressions into strict,
-- resulting in condiitional expression bugs
compact :: Var t -> Var t
compact (Var v) = Var (groupVals v (===))

compactEq :: Eq t => Var t -> Var t
compactEq (Var v) = Var (groupVals v (====))

valIndex :: Eq t => Var t -> t -> [Val t]
valIndex (Var v) x =
    filter (\(x',pc') -> x' == x) v

index :: Var t -> PresenceCondition -> [t]
index (Var v) pc = fst $ unzip v' 
    where   v' = filter (\(x',pc') -> sat (conj[pc,pc'])) v

subst :: Var t -> PresenceCondition -> Var t
subst (Var v) pc =
    Var (filter (\(_,pc') -> sat (conj [pc,pc'])) v)

definedAt :: Var t -> PresenceCondition
definedAt (Var xs) = disj(pcs)
    where   pcs     = map snd xs

undefinedAt :: Var t -> PresenceCondition
undefinedAt = neg . definedAt

restrict :: PresenceCondition -> Var t -> Var t
restrict pc (Var v) =
    Var $ filter (\(x,pc') -> sat pc') (map (\(x,pc') -> (x, conj[pc',pc])) v)
                                    
union :: Var t -> Var t -> Var t
union (Var a) (Var b) =
    Var (a ++ b)

unions :: [Var t] -> Var t 
unions xs = Var (foldr (++) [] (map (\(Var v) -> v) xs))

union2 :: Var (Var t) -> Var t 
union2 (Var xs') = unions (map (\(x,pc) -> (restrict pc x)) xs')

pairs :: [t] -> [(t,t)]
pairs [] = []
pairs xs = zip xs (tail xs)

inv :: Show t => Var t -> Bool
inv (Var v) = {-trace ("inv: " ++ (show (Var v))) $-} 
    all (\((_, pc1),(_, pc2)) -> unsat (conj[pc1, pc2])) (pairs v)

apply_ :: Val (a -> b) -> Var a -> Var b
apply_ (fn, fnpc) x'  = --localCtxt fnpc $
    let (Var x) = compact x'
    in mkVars [(fn x'', pc) | (x'',xpc) <- x, let pc = conj[fnpc,xpc], sat(pc)]

apply :: Var (a -> b) -> Var a -> Var b
apply (Var fn) x = --compact $
     unions [apply_ f x | f <- fn] 

-- lifting conditional expression
cond :: Bool -> a -> a -> a
cond p a b = if p then a else b

cond' :: Show a => Var Bool -> Var a -> Var a -> Var a
--cond' = liftV3 cond
cond' !(Var c) x y = compact agg
    where parts = map (\c' -> case c' of
                                (True, pc) -> restrict pc x
                                (False, pc) -> restrict pc y) c
          agg = foldr SPL.union (Var []) parts
         
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

--data VarOption a =
    
-- Bool operation lifting
(|==|) :: (Eq a) => Var a -> Var a -> Var Bool
(|==|) = liftV2 (==)

(|+|) :: Num a => Var a -> Var a -> Var a
(|+|) = liftV2 (+)

