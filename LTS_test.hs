-------------------------------------------------------------------------------
-- LTS_test.hs
-- Labeled Transition System (LTS) unit tests
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
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
       
notEmpty = not . null
-- LTS invariants
prop_inv_states         lts = notEmpty (getStates lts)
prop_inv_actions        lts = notEmpty (getActions lts)
prop_inv_transitions    lts = all (\(x,y) -> (elem x states) && (elem y states)) (getTransitions lts)
    where states = getStates lts
    

