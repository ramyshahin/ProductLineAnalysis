-------------------------------------------------------------------------------
-- Graph_test.hs
-- Graph (LTS) unit tests
-- Ramy Shahin - July 12th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graph_test where
import Test.QuickCheck
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
       