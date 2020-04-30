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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}

module SPL where

import PropBDD
import PresenceCondition
import Control.Applicative
import Control.Monad
import Data.List 
import Data.Maybe
import qualified Data.Vector as V 
import qualified Data.Set as S  
import Control.Exception
import Control.Parallel.Strategies
import System.Mem.StableName
import System.IO.Unsafe
import Debug.Trace

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
data Var t = Var [(Val t)]

disjInv :: Var t -> Bool
disjInv v'@(Var v) =
    let ret = all (\((_, pc1),(_, pc2)) -> unsat (conj[pc1, pc2])) (pairs v)
    in  if (not ret) then trace (showPCs v') ret else ret

compInv :: Var t -> Bool
compInv (Var v) =
    (foldr (\(_,pc) pc' -> pc \/ pc') ffPC v) == ttPC

inv :: Var t -> Bool
inv v = (disjInv v) && (compInv v)

exists :: Eq t => Val t -> Var t -> Bool
exists (x, xpc) ys' =
    or [(x == y) && (implies xpc ypc) | (y,ypc) <- ys]
    where (Var ys) = compactEq ys'

isSubsetOf :: Eq t => Var t -> Var t -> Bool
isSubsetOf (Var x) y' = and (map (`exists` y') x)

showPCs :: Var a -> String
showPCs (Var v) = "{" ++ (foldr (++) "" (map (\(x,pc) -> (show pc) ++ ", ") v)) ++ "}"

instance Show a => Show (Var a) where
    show v' = 
        let (Var v) = compact v' 
        in "{" ++ (foldr (++) "" (map (\x -> (show x) ++ ", ") v)) ++ "}" 

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
{-# INLINE mkVar #-}
mkVar v pc = Var [(v,pc)]

mkVarT :: t -> Var t
{-# INLINE mkVarT #-}
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
--compact (Var v) = Var (groupVals v (===))
compact = id 

compactEq :: Eq t => Var t -> Var t
--compactEq (Var v) = Var (groupVals v (====))
compactEq = id

valIndex :: Eq t => Var t -> t -> [Val t]
valIndex (Var v) x =
    filter (\(x',pc') -> x' == x) v

index :: Var t -> PresenceCondition -> [t]
index (Var v) pc = fst $ unzip v' 
    where   v' = filter (\(x',pc') -> sat (conj[pc,pc'])) v

configIndex :: Var t -> PresenceCondition -> t
configIndex v pc = 
    let r = index v pc
    in  assert (length r == 1) $ head r

subst :: PresenceCondition -> Var t -> Var t
subst pc (Var v) =
    Var (filter (\(_,pc') -> sat (conj [pc,pc'])) v)

definedAt :: Var t -> PresenceCondition
definedAt (Var xs) = disj(pcs)
    where   pcs     = map snd xs

undefinedAt :: Var t -> PresenceCondition
undefinedAt = neg . definedAt

restrict :: PresenceCondition -> Var t -> Var t
restrict pc (Var v) =
    Var $ filter (\(_,pc') -> sat pc') (map (\(x,pc') -> (x, conj[pc',pc])) v)
                                    
(/^) x pc = restrict pc x
(^|) x pc = mkVar x pc

--disjointnessInv :: Show t => Var t -> Var t -> Bool
--disjointnessInv x@(Var a) y@(Var b) = 
--    let conjunctions = [andBDD pc1 pc2 | (_,pc1) <- a, (_,pc2) <- b]
--    in  trace ((show x) ++ " U " ++ show y) $ all (== ff) conjunctions

tracePCs :: Var t -> String
tracePCs (Var xs) = foldl (\s (_,r) -> s ++ " " ++ (show r)) "" xs

getFeatures' :: Var t -> S.Set String
getFeatures' (Var vs) =
    foldr S.union S.empty (map (getPCFeatures' . snd) vs)

getFeatures :: Var t -> [String]
getFeatures = S.toList . getFeatures'

union :: Var t -> Var t -> Var t
union x@(Var a) y@(Var b) =
    let result = Var (a ++ b)
    in {-trace (tracePCs x) $ trace (tracePCs y) $ assert (inv result)-} result

unions :: [Var t] -> Var t 
unions xs = foldr SPL.union (Var []) xs

--union2 :: Var (Var t) -> Var t 
--union2 (Var xs') = unions (map (\(x,pc) -> (restrict pc x)) xs')

pairs :: [t] -> [(t,t)]
pairs [] = []
pairs xs = zip xs (tail xs)

apply_ :: Val (a -> b) -> Var a -> Var b
--{-# INLINE apply_ #-}
apply_ (fn, fnpc) x'@(Var x)  = --localCtxt fnpc $
    let xs = filter (\(_, pc) -> sat (fnpc /\ pc)) x in 
    mkVars $ map (\(v, pc) -> (fn v, fnpc /\ pc)) xs

--{-# INLINE apply #-}
apply :: Var (a -> b) -> Var a -> Var b
apply f@(Var fn) x = --assert (disjInv f && disjInv x) $ --compact $
     unions [apply_ f x | f <- fn] 

-- lifting conditional expression
cond :: Bool -> a -> a -> a
cond p a b = if p then a else b

evalCond :: Var Bool -> (PresenceCondition, PresenceCondition)
evalCond (Var c) = 
    let t = filter (\(v,pc) -> v == True) c
        f = filter (\(v,pc) -> v == False) c
        tPC = foldr (\(_, pc) x -> x \/ pc) ffPC t
        fPC = foldr (\(_, pc) x -> x \/ pc) ffPC f
    in (tPC, fPC)

liftedCond :: Var Bool -> (PresenceCondition -> Var a) -> (PresenceCondition -> Var a) -> Var a
liftedCond c x y = 
    let (t,f) = evalCond c
    in SPL.union (x t) (y f)

{-
liftedCond c'@(Var c) x y = --assert (disjInv c' && disjInv x && disjInv y) $ 
    --liftV3 cond
    compact agg
    where parts = map (\c' -> case c' of
                                    (True, pc)  -> (mkVar id pc) <*> (subst pc x)
                                    (False, pc) -> (mkVar id pc) <*> (subst pc y))
                      c
          agg = unions parts
-}
liftedNeg :: Num a => Var (a -> a)
liftedNeg = mkVarT (\x -> -x)

partitionInv :: Var a -> [Var a] -> Bool
partitionInv (Var x) xs = length x == sum xs
    where sum xs = foldl (\c (Var x) -> c + length x) 0 xs

caseSplitter :: Var a -> (a -> Int) -> Int -> [Var a]
caseSplitter i@(Var input) splitter range = --assert (compInv i) $
    let initV = V.replicate range (Var [])
        xs = foldl 
                (\vec (v,pc) -> let index = splitter v 
                                    (Var item)  = vec V.! index
                                    item' = Var $ (v, pc) : item
                                in  vec V.// [(index, item')]) 
                initV input 
        ret = V.toList xs
    in  --trace (foldl (++) "splits:\t" (map showPCs ret)) $ 
        assert (partitionInv i ret) ret

isNilVar :: Var a -> Bool
isNilVar (Var xs) = null xs 

liftedCase :: Var a -> (a -> Int) -> [PresenceCondition -> Var a -> Var b] -> Var b
liftedCase input splitter alts = --trace ("In: " ++ showPCs input) $ assert (compInv input) $ 
    --trace (foldl (++) "parts:\t" (map showPCs parts)) $ 
    --trace ("Out: " ++ showPCs agg) $
    assert (disjInv agg) agg
    where   split = caseSplitter input splitter (length alts) 
            parts  = map (\(l,r) -> let ret = if isNilVar r then (Var []) else l (definedAt r) r
                                    in  {-trace ("\t\t\tBlabla:" ++ (showPCs r) ++ "\t" ++ (showPCs ret))-} ret) (zip alts split) 
            agg    = unions parts  

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

-- lifted list
--data [a]^ = 
--    [^]
--   | (Var a) :^ (List' a)

--pattern (:^) :: Var a -> List' a -> List' a 
--pattern x :^ xs <- Cons' x xs
--infixr :^

(|:|) :: Var a -> Var [a] -> Var [a]
(|:|) = liftV2 (:)
infixr 5 |:|

-- lifted primitive operators
--(:^) :: Var a -> Var [a] -> Var [a]
(^:) = (|:|) --liftV2 (:)
infixr 5 ^:

(^+) :: Num a => Var a -> Var a -> Var a
(^+) = liftV2 (+)
infixl 6 ^+

(^-) :: Num a => Var a -> Var a -> Var a
(^-) = liftV2 (-)
infixl 6 ^-

(^*) :: Num a => Var a -> Var a -> Var a
(^*) = liftV2 (*)
infixl 7 ^*

(^/) :: Fractional a => Var a -> Var a -> Var a
(^/) = liftV2 (/)
infixl 7 ^/

(^==) :: Eq a => Var a -> Var a -> Var Bool
(^==) = liftV2 (==)
infix 3 ^==

(^/=) :: Eq a => Var a -> Var a -> Var Bool
(^/=) = liftV2 (/=)
infix 3 ^/=

(^&&) :: Var Bool -> Var Bool -> Var Bool
(^&&) = liftV2 (&&)
infixr 3 ^&&

(^||) :: Var Bool -> Var Bool -> Var Bool
(^||) = liftV2 (||)
infixr 2 ^||

primitiveOpNames :: S.Set String
primitiveOpNames = S.fromList [":", "+", "-", "*", "/", "==", "/=", "&&", "||", "."]

primitiveFuncNames :: S.Set String
primitiveFuncNames = S.fromList ["not", "head", "tail", "null", "fst", "snd", "map", "filter", "foldr", "foldl"]

ttPC = tt
ffPC = ff

-- lifted primitive functions
not' :: Var Bool -> Var Bool
not' = liftV not

head' :: Var [a] -> Var a
head' = liftV head

tail' :: Var [a] -> Var [a]
tail' = liftV tail

null' :: Foldable t => Var (t a) -> Var Bool
null' = liftV null

fst' :: Var (a, b) -> Var a
fst' = liftV fst

snd' :: Var (a, b) -> Var b
snd' = liftV snd

#ifdef SHALLOW

(^.) :: Var (b -> c) -> Var (a -> b) -> Var a -> Var c
(^.) = liftV3 (.)
infixr 9 ^.

map' :: Var (a -> b) -> Var [a] -> Var [b]
map' = liftV2 map

filter' :: Var (a -> Bool) -> Var [a] -> Var [a]
filter' = liftV2 filter

foldr' :: Var (a -> b -> b) -> Var b -> Var [a] -> Var b
foldr' = liftV3 foldr

foldl' :: Var (b -> a -> b) -> Var b -> Var [a] -> Var b
foldl' = liftV3 foldl

#endif

----- tuple access
oneOfOne :: (a) -> a
oneOfOne (x) = x
oneOfOne' = liftV oneOfOne

oneOfTwo :: (a,b) -> a
oneOfTwo = fst
oneOfTwo' = fst'

twoOfTwo :: (a,b) -> b
twoOfTwo = snd
twoOfTwo' = snd'

oneOfThree :: (a,b,c) -> a
oneOfThree (x,y,z) = x
oneOfThree' = liftV oneOfThree

twoOfThree :: (a,b,c) -> b
twoOfThree (x,y,z) = y
twoOfThree' = liftV twoOfThree

threeOfThree :: (a,b,c) -> c
threeOfThree (x,y,z) = z
threeOfThree' = liftV threeOfThree

uncurry0 :: Var a -> Var () -> Var a
uncurry0 x _ = x

uncurry1 :: (Var a -> Var b) -> Var (a) -> Var b
uncurry1 f x = f (oneOfOne' x)

uncurry2 :: (Var a -> Var b -> Var c) -> Var (a,b) -> Var c
uncurry2 f x = f (oneOfTwo' x) (twoOfTwo' x)

uncurry3 :: (Var a -> Var b -> Var c -> Var d) -> Var (a, b, c) -> Var d
uncurry3 f x = f (oneOfThree' x) (twoOfThree' x) (threeOfThree' x)

-- testing only
{-
_length xs = liftedCond (null' xs) (mkVarT 0) ((mkVarT 1) ^+ (_length (tail' xs)))
univ@[p, q, r, s] = mkUniverse ["P", "Q", "R", "S"]
pq = conj[p,q]
p_q = conj[p, neg q]
_pq = conj[neg p, q]
_p_q = conj[neg p, neg q]
_p = neg p
_q = neg q
x = mkVars [(7, pq), (-3, p_q), (-8, _pq), (0, _p_q)]
xs = mkVars [([1,2,3,4], p), ([3,2], _p)]

list0 = mkVarT []
list1 = x ^: list0
-}