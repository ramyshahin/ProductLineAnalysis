-------------------------------------------------------------------------------
-- SPL.hs
-- Software Product Line library
-- Ramy Shahin - July 14th 2016
-------------------------------------------------------------------------------
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances, ExplicitForAll #-}

module SPL where

import PropBDD
import PresenceCondition
import Control.Applicative
import Control.Monad
import Data.List 
import Data.Maybe
import qualified Data.Vector    as V 
import qualified Data.Set       as S  
import qualified Data.List      as L
import Control.Exception
import Control.Parallel.Strategies
import System.Mem.StableName
import System.IO.Unsafe
import Debug.Trace
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

{-# INLINE (===) #-}
(===) :: a -> a -> Bool
(!x) === (!y) = unsafePerformIO $ do 
    nx <- makeStableName $! x 
    ny <- makeStableName $! y 
    return (nx == ny)

(====) :: Eq a => a -> a -> Bool
(====) x y = x === y || x == y

--type FeatureSet         = Universe
type PresenceCondition  = Prop

type Context = PresenceCondition

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
    deriving (Generic, NFData)

--instance NFData (Var a) where
--    rnf (Var !xs) = xs `seq` (map (\(!x,!pc) -> x `seq` pc `seq` ()) xs) `seq` ()

disjInv :: Var t -> Bool
disjInv v'@(Var v) =
    let ret = all (\((_, pc1),(_, pc2)) -> unsat (pc1 /\ pc2)) (pairs v)
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
showPCs (Var v) = "{" ++ (L.intercalate ", " (map (\(_,pc) -> show pc) v)) ++ "}"

instance Show a => Show (Var a) where
    show v' = 
        let (Var v) = compact v' 
        in "{" ++ (L.intercalate ", " (map show v)) ++ "}" 

-- a < b means that a is a subset of b in terms of products
--instance Eq a => Ord (Var a) where
--    (<) x' y' = (isSubsetOf x' y') && not (isSubsetOf y' x')
--    (<=) x y = (x < y) || (x == y)

--instance Eq a => Eq (Var a) where
--    (==) x y = (isSubsetOf x y) && (isSubsetOf y x)
--    (/=) x y = not (x == y)

instance Functor Var where
    fmap :: (a -> b) -> Var a -> Var b
    fmap f = apply (f ^| ttPC)

instance Applicative Var where
    pure  = (^| ttPC)
    (<*>) = apply

{-
-- Var monad
data VarM a =
    VarM (Context -> (a, Context))

instance Functor VarM where
    fmap :: (a -> b) -> VarM a -> VarM b
    fmap f (VarM t) = VarM (\c -> let (x, c') = t c in (f x, c'))

instance Applicative VarM where
    pure :: a -> VarM a
    pure x = VarM (\c -> (x, c))
    
    (<*>) :: VarM (a -> b) -> VarM a -> VarM b
    (VarM l) <*> (VarM r) = VarM (\c -> let (v, c')  = l c
                                            (w, c'') = r c'
                                        in (v w, c''))

instance Monad VarM where
    (>>=) :: VarM a -> (a -> VarM b) -> VarM b
    VarM l >>= t = VarM
        (\c -> let (v, c')  = l c
                   (VarM y) = t v
               in y c')
    
    (>>) :: VarM a -> VarM b -> VarM b
    l >> r = r

    return  = pure
    fail    = error
-}

--type family Var t where
--    Var ((t :: * -> * -> * -> *) (s1 :: *) (s2 :: *) (s3 :: *))      = Var' (t (Var' s1) (Var' s2) (Var' s3))
--    Var ((t :: * -> * -> *) (s1 :: *) (s2 :: *))      = Var' (t (Var' s1) (Var' s2))
--    Var ((t :: * -> *) (s :: *))      = Var' (t (Var' s))
--    Var (t :: *)                      = Var' t

mkVar :: PresenceCondition -> t -> Var t
{-# INLINE mkVar #-}
mkVar pc v = Var [(v,pc)]

(^|) :: t -> PresenceCondition -> Var t
x ^| pc = mkVar pc x
infixl 9 ^|

mkVarT :: a -> Var a
{-# INLINE mkVarT #-}
mkVarT v = v ^| tt

mkVars :: [(t,PresenceCondition)] -> Var t
mkVars vs = Var vs

{-# INLINE findVal #-}
findVal :: t -> [Val t] -> (t -> t -> Bool) -> [Val t]
findVal _ [] _ = []
findVal v ((x,pc):xs) cmp = if (cmp v x) then (x,pc) : rest else rest
    where rest = findVal v xs cmp

{-# INLINE phelem #-}
phelem :: t -> [t] -> (t -> t -> Bool) -> Bool
phelem v xs cmp = any (\x -> cmp v x) xs

{-# INLINE groupVals_ #-}
groupVals_ :: [Val t] -> [t] -> (t -> t -> Bool) -> [Val t]
groupVals_ [] _ _ = []
groupVals_ ((x,xpc):xs) ds cmp = 
    if phelem x ds cmp then rest else 
        let ms = findVal x xs cmp
            pc = disj(xpc:(snd . unzip) ms)
        in  (x,pc) : rest
    where rest = groupVals_ xs (x:ds) cmp

{-# INLINE groupVals #-}
groupVals :: [Val t] -> (t -> t -> Bool) -> [Val t]
groupVals xs cmp = groupVals_ xs [] cmp

{-# INLINE compact #-}
compact :: Var t -> Var t
compact (Var v) = Var (groupVals v (===))

compactEq :: Eq t => Var t -> Var t
--compactEq (Var v) = Var (groupVals v (====))
compactEq = id

valIndex :: Eq t => Var t -> t -> [Val t]
valIndex (Var v) x =
    filter (\(x',pc') -> x' == x) v

index :: Var t -> PresenceCondition -> [t]
index (Var v) pc = fst $ unzip v' 
    where   v' = filter (\(x',pc') -> sat (pc /\ pc')) v

configIndex :: Var t -> PresenceCondition -> t
configIndex v pc = 
    let r = index v pc
    in  assert (length r == 1) $ head r

subst :: PresenceCondition -> Var t -> Var t
subst pc (Var v) =
    Var (filter (\(_,pc') -> sat (pc /\ pc')) v)

definedAt :: Var t -> PresenceCondition
definedAt (Var xs) = disj(pcs)
    where   pcs     = map snd xs

undefinedAt :: Var t -> PresenceCondition
undefinedAt = neg . definedAt

{-# INLINE restrict #-}
restrict :: PresenceCondition -> Var t -> Var t
restrict pc v'@(Var v) =
    if      pc == ttPC then v'
    else if pc == ffPC then Var []
    else    Var [(x, p) | (x, pc') <- v, let p = pc' /\ pc, sat p]
    --Var $ filter (\(_,pc') -> sat pc') (map (\(x,pc') -> (x, pc'/\ pc)) v)
                                    
(/^) x pc = restrict pc x
(^|) x pc = mkVar x pc

--disjointnessInv :: Show t => Var t -> Var t -> Bool
--disjointnessInv x@(Var a) y@(Var b) = 
--    let conjunctions = [andBDD pc1 pc2 | (_,pc1) <- a, (_,pc2) <- b]
--    in  trace ((show x) ++ " U " ++ show y) $ all (== ff) conjunctions

tracePCs :: Var t -> String
tracePCs (Var xs) = foldl (\s (_,r) -> s ++ " " ++ (show r)) "" xs

{-
getFeatures' :: Var t -> S.Set String
getFeatures' (Var vs) =
    foldr S.union S.empty (map (getPCFeatures' . p . snd) vs)
-}

getFeatures :: IO [String]
getFeatures = do 
    !vs <- getVars ttPC
    return $ (fst . unzip) vs

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

{-# INLINE apply_ #-}
apply_ :: Val (a -> b) -> Var a -> Var b
apply_ (fn, !fnpc) x'@(Var x)  = --localCtxt fnpc $
    mkVars $ [(fn v, pc') | (v, !pc) <- x, let !pc' = fnpc /\ pc, sat pc'] --map (\(v, pc) -> ) xs
        --xs = filter (\(_, pc) -> sat (fnpc /\ pc)) x in 
    

{-# INLINE apply #-}
apply :: Var (a -> b) -> Var a -> Var b
apply f@(Var fn) x = assert (disjInv f) $
     assert (disjInv x) $ --compact $
     unions [apply_ f x | f <- fn] 

-- VClass type class
class VClass a where
    nil  :: a b
    isNil:: a b -> Bool
    at   :: a b -> PresenceCondition
    comb :: a b -> a b -> a b 
    caseSplitter :: a b -> (b -> Int) -> Int -> [a b] 
    combs :: [a b] -> a b
    combs = foldr comb nil

--instance Foldable Var where
--    foldMap f (Var xs) = foldMap (\(x,pc) -> (f x, pc)) xs

instance VClass Var where
    nil  = Var []
    isNil (Var xs) = null xs 
    at   = definedAt
    --cons v pc (Var vs) = Var $ (v,pc) : vs
    comb = SPL.union

    caseSplitter i@(Var input) splitter range = --assert (compInv i) $
        let initV = V.replicate range nil
            xs = foldl 
                    (\vec (v, pc) -> let index = splitter v 
                                         (Var item)  = vec V.! index
                                         item' = Var $ (v,pc) : item
                                     in  vec V.// [(index, item')]) 
                 initV input 
            ret = V.toList xs
        in  --trace (foldl (++) "splits:\t" (map showPCs ret)) $ 
            assert (partitionInv i ret) ret

--newtype VDeep a = VDeep a

--instance Foldable VList where
--    foldMap f (VList l) = f (l, ttPC)

{-
instance VClass [a] where -- VList where
    nil  = VDeep []
    isNil _ = False
    at _ = ttPC 
    --cons x pc (VList xs) = VList $ (x ^| pc) : xs
    comb (VDeep xs) (VDeep ys) = VDeep (xs ++ ys) -- (VList a) (VList b) = VList (a ++ b)

    --caseSplitter i splitter range = -- BUGBUG
    --    let r = caseSplitter (Var [(i,ttPC)]) (\x -> splitter [x]) range
    --    in  map (\(Var vs) -> (fst . unzip) vs) r 
        --let initV = V.replicate range nil
        --    index = splitter input
        --    v'    = initV V.// [(index, i)]
        --in  V.toList v'
-}

-- lifting conditional expression
cond :: Bool -> a -> a -> a
cond p a b = if p then a else b

evalCond :: Var Bool -> (PresenceCondition, PresenceCondition)
evalCond c'@(Var c) = 
    let t = filter (\(v,pc) -> v == True) c
        f = filter (\(v,pc) -> v == False) c
        tPC = foldr (\(_, pc) x -> x \/ pc) ffPC t
        fPC = foldr (\(_, pc) x -> x \/ pc) ffPC f
    in  --trace ("tPC: " ++ (show tPC)) $ 
        --trace ("fPC: " ++ (show fPC)) $
        assert (tPC /\ fPC == ffPC) $
        assert (tPC \/ fPC == definedAt c') $
        (tPC, fPC)

liftedCond :: VClass a => Var Bool -> (PresenceCondition -> a b) -> (PresenceCondition -> a b) -> a b
liftedCond c x y = 
    let (t,f) = evalCond c
    in  if t == ffPC then (y f)
        else if f == ffPC then (x t)
        else comb (x t) (y f)

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
neg' :: Num a => Var a -> Var a
neg' = liftV (\x -> -x)

partitionInv :: Var a -> [Var a] -> Bool
partitionInv x xs = (definedAt x) == cover
    where cover = foldr (\/) ffPC (map definedAt xs)

    {-
caseSplitter :: Var a -> (a -> Int) -> Int -> [Var a]
caseSplitter i@(Var input) splitter range = --assert (compInv i) $
    let initV = V.replicate range (Var [])
        xs = foldl 
                (\vec (v, pc) -> let index = splitter v 
                                     (Var item)  = vec V.! index
                                     item' = Var $ (v,pc) : item
                                  in  vec V.// [(index, item')]) 
                initV input 
        ret = V.toList xs
    in  --trace (foldl (++) "splits:\t" (map showPCs ret)) $ 
        assert (partitionInv i ret) ret
-}
isNilVar :: Var a -> Bool
isNilVar (Var xs) = null xs 

liftedCase :: (VClass v, VClass u) => v a -> (a -> Int) -> [PresenceCondition -> v a -> u b] -> u b
liftedCase input splitter alts = --assert (not (isNilVar input)) $
    --trace ("In: " ++ showPCs input) $ assert (disjInv input) $ 
    --trace (foldl (++) "parts:\t" (map showPCs parts)) $ 
    --trace ("Out: " ++ showPCs agg) $
    --assert (disjInv agg) $ 
    agg
    where   split = caseSplitter input splitter (length alts) 
            parts  = map (\(l,r) -> let ret = if (isNil r) then nil else l (at r) r
                                    in  --trace ((show (definedAt r)) ++ "\t" ++ (show (definedAt ret))) $ 
                                        --assert (definedAt ret == definedAt r) $ 
                                        ret) (zip alts split) 
            agg    = combs parts  

fixCompleteness :: a -> Var a -> Var a
fixCompleteness dummy v =
    let r = undefinedAt v
    in  if r == ffPC then v else SPL.union v $ mkVar dummy r

-- lifting higher-order functions
mapLifted :: Var (a -> b) -> Var [a] -> Var [b]
mapLifted = liftA2 map

filterLifted :: Var (a -> Bool) -> Var [a] -> Var [a]
filterLifted = liftA2 filter

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = fmap f a <*> b <*> c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = fmap f a <*> b <*> c <*> d <*> e

{-# INLINE liftV #-}
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

{-# INLINE (|:|) #-}
(|:|) :: Var a -> Var [a] -> Var [a]
(|:|) = liftV2 (:)
infixr 5 |:|

-- lifted primitive operators
(^:) :: Var a -> [Var a] -> [Var a]
(^:) = (:)
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

(^++) :: [Var a] -> [Var a] -> [Var a]
(^++) = (++)
infixr 5 ^++

primitiveOpNames :: S.Set String
--primitiveOpNames = S.fromList [":", "+", "-", "*", "/", "==", "/=", "&&", "||", "."]
primitiveOpNames = S.fromList [".", ":", "++"]

primitiveFuncNames :: S.Set String
primitiveFuncNames = S.fromList ["not", "head", "tail", "null", "fst", "snd", "map", "filter", "foldr", "foldl", "length",
                                    "_succs", "_nodes", "_nID"]

ttPC = tt
ffPC = ff

-- lifted primitive functions
not' :: Var Bool -> Var Bool
not' = liftV not

head' :: [Var a] -> Var a
head' = head --liftV head

tail' :: [Var a] -> [Var a]
tail' = tail

null' :: Foldable t => Var (t a) -> Var Bool
null' = liftV null

fst' :: Var (a, b) -> Var a
fst' = liftV fst

snd' :: Var (a, b) -> Var b
snd' = liftV snd

--ifdef SHALLOW

--(^.) :: (Var b -> Var c) -> (Var a -> Var b) -> Var a -> Var c
(^.) = (.)
infixr 9 ^.

map' :: (Var a -> Var b) -> [Var a] -> [Var b]
map' = map

filter' :: (Var a -> Var Bool) -> [Var a] -> [Var a]
filter' p [] = []
filter' p (x : xs) = 
    let r@(Var r') = liftedCond (p x) (\pc -> x /^ pc) (\pc -> (Var []) /^ pc)
    in  if null r' then filter' p xs else r : (filter' p xs)

foldr' :: (Var a -> Var b -> Var b) -> Var b -> [Var a] -> Var b
foldr' f z xs = 
    let ret = foldr f z xs
    in  if definedAt ret == ttPC then ret
        else SPL.union ret (z /^ (undefinedAt ret))

foldl' :: (Var b -> Var a -> Var b) -> Var b -> [Var a] -> Var b
foldl' = foldl

length' :: [Var a] -> Var Integer
length' [] = (0 ^| ttPC)
length' (x : xs) = 
    (Var [(1, definedAt x), (0, undefinedAt x)]) ^+ length' xs

-- #endif

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
