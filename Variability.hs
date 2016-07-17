-------------------------------------------------------------------------------
-- Variability.hs
-- Software Product Line Variability library
-- Ramy Shahin - July 14th 2016
-------------------------------------------------------------------------------
module Variability where

-- abstract propositional expression
type Expr = Bool

type PresenceCondition  = Expr
type SPLContext         = Expr

sat :: Expr -> Bool
sat ex = True

data SPLVariable a = 
    SPLVariable a PresenceCondition
    deriving (Eq) 

-- when lifting a product value to a product line value, we might end up with
-- different values for each product in the product line. This is why a value is
-- lifted into a set of values, each with a path condition. An important
-- invariant here is that the path conditions of those values should not depend
-- on each other, i.e. non of them logically implies the other. Violating this
-- invariant would result in redundant values (i.e. multiple values belonging
-- to the same set of products). This does not affect correctness, but severely
-- affects performance as we are now degenerating into brute force analysis
-- across all possible products.
type SPLLifted a = [SPLVariable a]

get :: SPLContext -> SPLVariable a -> Maybe a
get cntxt v = case v of
    SPLVariable val pc -> if sat (cntxt && pc) then Just val else Nothing
    
--data VarOption a =
    
