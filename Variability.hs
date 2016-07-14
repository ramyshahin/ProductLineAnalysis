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

get :: SPLContext -> SPLVariable a -> Maybe a
get cntxt v = case v of
    SPLVariable val pc -> if sat (cntxt && pc) then Just val else Nothing
    
--data VarOption a =
    