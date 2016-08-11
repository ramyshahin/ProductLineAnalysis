-------------------------------------------------------------------------------
-- Variability.hs
-- Software Product Line Variability library
-- Ramy Shahin - July 14th 2016
-------------------------------------------------------------------------------
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
    
-- lift a value
liftValue :: a -> Lifted a
liftValue x = [(x, True)]

-- join (Lifted-Lifted) - join 2 lifted values
join2 :: PresenceCondition -> Lifted a -> Lifted b -> Lifted (a,b)
join2 pc a b = 
    let xProduct = [((aVal, bVal), (conj [pc, aPC, bPC])) | (aVal, aPC) <- a, (bVal, bPC) <- b]
    in  filter (\(_, pc) -> sat pc) xProduct
    
join3 :: PresenceCondition -> Lifted a -> Lifted b -> Lifted c -> Lifted (a,b,c)
join3 pc a b c = 
    let xProduct = [((aVal, bVal, cVal), (conj [pc, aPC, bPC, cPC])) | (aVal, aPC) <- a, (bVal, bPC) <- b, (cVal, cPC) <- c]
    in  filter (\(_, pc) -> sat pc) xProduct
    
apply2 :: PresenceCondition -> (a -> b -> c) -> Lifted a -> Lifted b -> Lifted c
apply2 cntxt fn a b = [((fn x y), pc) | ((x,y), pc) <- (join2 cntxt a b)]

apply3 :: PresenceCondition -> (a -> b -> c -> d) -> Lifted a -> Lifted b -> Lifted c -> Lifted d
apply3 cntxt fn a b c = [((fn x y z), pc) | ((x,y,z), pc) <- (join3 cntxt a b c)]

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
    
