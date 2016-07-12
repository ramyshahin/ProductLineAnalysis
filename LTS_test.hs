-------------------------------------------------------------------------------
-- LTS_test.hs
-- Labeled Transition System (LTS) unit tests
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module LTS_test where
import Test.QuickCheck
import LTS
import Data.Graph
import Graph_test

-- Generators
instance Arbitrary LTS where
    arbitrary = do
       graph <- arbitrary
       actions <- arbitrary
       let states = vertices graph
       initStates <- sublistOf states
       return (LTS graph actions initStates)
      
-- helper functions      
notEmpty = not . null


-- LTS invariants
prop_inv_states         lts = notEmpty (getStates lts)
-- TODO prop_inv_actions        lts = notEmpty (getActions lts)
prop_inv_transitions    lts = all (\(x,y) -> (elem x states) && (elem y states)) (getTransitions lts)
    where states = getStates lts

prop_inv_initStates     lts = all (\x -> elem x states) initStates
    where   states = getStates lts
            initStates = getInitStates lts

-------------------------------------------------------------------------------
-- LTS Algorithm Tests
-------------------------------------------------------------------------------
prop_reachability   lts s =
    elem s (getStates lts) ==>
        isReachable lts s == elem s reachableStates
            where   reachableStates = foldl (++) [] reachables
                    reachables      = map (reachable graph) initStates
                    graph           = getGraph lts
                    initStates      = getInitStates lts
                    
return [] -- need this for GHC 7.8

runTests = $quickCheckAll
