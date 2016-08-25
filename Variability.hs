-------------------------------------------------------------------------------
-- Variability.hs
-- Software Product Line Variability library
-- Ramy Shahin - July 14th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE #-}

module Variability where

-- abstract propositional expression
type Expr = Bool

type PresenceCondition  = Expr
--type SPLContext         = Expr

conj :: [PresenceCondition] -> PresenceCondition
conj a = True     -- TODO

disj :: Expr -> Expr -> Expr
disj a b = True     -- TODO

sat :: Expr -> Bool
sat ex = True

type SPLOption a = (a, PresenceCondition)

--instance Eq SPLOption a where
--    (==) a b = (getValue a == getValue b) && sat(getPresenceCondition a && getPresenceCondition b)


-- when lifting a product value to a product line value, we might end up with
-- different values for each product in the product line. This is why a value is
-- lifted into a set of values, each with a path condition. An important
-- invariant here is that the path conditions of those values should not depend
-- on each other, i.e. non of them logically implies the other. Violating this
-- invariant would result in redundant values (i.e. multiple values belonging
-- to the same set of products). This does not affect correctness, but severely
-- affects performance as we are now degenerating into brute force analysis
-- across all possible products.
type Lifted a = [SPLOption a]
--type Lifted2 a b = Lifted (a (Lifted b))
--type Lifted3 a b c = Lifted (a (Lifted (b (Lifted c))))

--data Proxy t = Proxy

--class CLifted t where
--  lift :: Proxy t -> TypeRep

--instance CLifted Int  where typeOf _ = TypeRep
--instance Typeable []   where typeOf _ = TypeRep

--newtype Lifted a b = Lifted a (Lifted b)

--type Lifted2 a b = Lifted (a (Lifted b))

-- join 2 lifted values
--join :: Lifted a -> Lifted b -> 
-- lift a value
lift :: PresenceCondition -> a -> Lifted a
lift pc x = [(x, pc)]

liftT :: a -> Lifted a
liftT x = lift True x

--lift2 :: Functor a => PresenceCondition -> a b -> Lifted2 a b
--lift2 pc x = [(fmap (lift pc) x, pc)]

--lift2 :: PresenceCondition -> a b -> Lifted (a (Lifted b))

-- apply a unary lifted function
apply :: Lifted (a -> b) -> Lifted a -> Lifted b
apply fn a = [(fn' a', conj [fnpc,apc]) | (fn',fnpc) <- fn, (a',apc) <- a, sat(conj[fnpc,apc])] 

-- apply a binary lifted function
apply2 :: Lifted (a -> b -> c) -> Lifted a -> Lifted b -> Lifted c
apply2 fn a = apply (apply fn a)

-- apply a ternary lifted function
apply3 :: Lifted (a -> b -> c -> d) -> Lifted a -> Lifted b -> Lifted c -> Lifted d
apply3 fn a b = apply (apply2 fn a b)

-- apply a 4-arity lifted function
apply4 :: Lifted (a -> b -> c -> d -> e) -> Lifted a -> Lifted b -> Lifted c -> Lifted d -> Lifted e
apply4 fn a b c = apply (apply3 fn a b c)

-- lifting conditional expression
cond :: Bool -> a -> a -> a
cond p a b = if p then a else b

condLifted = apply3 (lift True cond)

-- lifting higher-order functions
mapLifted :: Lifted (a -> b) -> Lifted [a] -> Lifted [b]
mapLifted = apply2 (lift True map)

filterLifted :: Lifted (a -> Bool) -> Lifted [a] -> Lifted [a]
filterLifted = apply2 (lift True filter)

-- lifted list
consLifted :: Lifted a -> Lifted [a] -> Lifted [a]
consLifted x xs = apply2 (liftT (:)) x xs

--consListLifted :: PresenceCondition -> Lifted a -> ListLifted a -> ListLifted a
--consListLifted pc x xs = consLifted x xs

headLifted :: Lifted [a] -> Lifted a
headLifted xs = apply (liftT head) xs

tailLifted :: Lifted [a] -> Lifted [a]
tailLifted xs = apply (liftT tail) xs

--lift1 :: PresenceCondition -> (a -> b) -> (Lifted a -> Lifted b)
--lift1 pc fn = (filter (\(v,pc') -> sat pc')) . map (\(v,pc') -> (fn v, (conj [pc, pc'])))

--lift2 :: PresenceCondition -> (a -> b -> c) -> (Lifted a -> Lifted b -> Lifted c)
--lift2 pc fn = {-(filter (\(v,pc') -> sat pc')) . foldr (++) [] . -} (map (\(v,pc') -> (lift1 pc (fn v))))

-- join (Lifted-Lifted) - join 2 lifted values
join2 :: PresenceCondition -> Lifted a -> Lifted b -> Lifted (a,b)
join2 pc a b = 
    let xProduct = [((aVal, bVal), (conj [pc, aPC, bPC])) | (aVal, aPC) <- a, (bVal, bPC) <- b]
    in  filter (\(_, pc) -> sat pc) xProduct
    
join3 :: PresenceCondition -> Lifted a -> Lifted b -> Lifted c -> Lifted (a,b,c)
join3 pc a b c = 
    let xProduct = [((aVal, bVal, cVal), (conj [pc, aPC, bPC, cPC])) | (aVal, aPC) <- a, (bVal, bPC) <- b, (cVal, cPC) <- c]
    in  filter (\(_, pc) -> sat pc) xProduct

{-
apply2 :: PresenceCondition -> (a -> b -> c) -> Lifted a -> Lifted b -> Lifted c
apply2 cntxt fn a b = [((fn x y), pc) | ((x,y), pc) <- (join2 cntxt a b)]

apply3 :: PresenceCondition -> (a -> b -> c -> d) -> Lifted a -> Lifted b -> Lifted c -> Lifted d
apply3 cntxt fn a b c = [((fn x y z), pc) | ((x,y,z), pc) <- (join3 cntxt a b c)]
-}

-- joinUL (Unlifted-Lifted) - join an unlifted value with a lifted value
{- joinUL :: a -> Lifted b -> Lifted (a,b)
joinUL a b =
    let xProduct = [SPLOption (a, bVal) bPC | (SPLOption bVal bPC) <- b]
    in  filter (\(SPLOption _ pc) -> sat pc) xProduct -}
    
-- joinLU (Lifted-Unlifted) - join an lifed value with a unlifted value
{-joinLU :: Lifted a -> b -> Lifted (a,b)
joinLU a b =
    let xProduct = [SPLOption (aVal, b) aPC | (SPLOption aVal aPC) <- a]
    in  filter (\(SPLOption _ pc) -> sat pc) xProduct -}
    
--liftVal :: a -> Lifted a
--liftVal a = [(a,True)]

--liftFun :: (a -> b -> c) -> Lifted a -> Lifted b -> Lifted c
--liftFun fn a b = 
--    map (\(SPLOption(x,y) pc) -> SPLOption (fn x y) pc) (joinLL a b)
  
--get :: SPLContext -> SPLOption a -> Maybe a
--get cntxt v = case v of
--    SPLOption val pc -> if sat (cntxt && pc) then Just val else Nothing
    
--data VarOption a =
    
