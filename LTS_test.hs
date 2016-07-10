-------------------------------------------------------------------------------
-- LTS_test.hs
-- Labeled Transition System (LTS) unit tests
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import LTS
import Data.Graph
import Data.List

--
-- Generators
--
generateBounds :: Gen Bounds
generateBounds = do
    let maxIndex = 100
    a <- choose(0,maxIndex)
    b <- choose(a,maxIndex)
    return (a,b)
    
generateEdge :: Bounds -> Gen Edge
generateEdge bounds = do
    u <- choose bounds
    v <- choose bounds
    return (u,v)
    
instance Arbitrary Graph where
    arbitrary = do
       bounds <- generateBounds
       let vertexCount = (snd bounds) - (fst bounds) + 1
       let edgeGenerator = generateEdge bounds
       edgeCount <- choose (1,vertexCount*vertexCount)
       edges <- (sequence [edgeGenerator | _ <- [1..edgeCount]])
       return (buildG bounds (nub edges))
        
notEmpty = not . null
-- LTS invariants
prop_inv_states         lts = notEmpty (getStates lts)
prop_inv_actions        lts = notEmpty (getActions lts)
prop_inv_transitions    lts = all (\(x,y) -> (elem x states) && (elem y states)) (getTransitions lts)
    where states = getStates lts
    

-- dummy tests
g = buildG (1,9) [(3,6), (3,2), (1,7)]
lts = LTS g [] [3]

