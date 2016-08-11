-------------------------------------------------------------------------------
-- LTS_test.hs
-- Labeled Transition System (LTS) unit tests
-- Ramy Shahin - July 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module LTS_test where
import Test.QuickCheck
import LTS
import Data.List
{-
-- Generators
instance Arbitrary LTS where
    arbitrary = do
       states       <- arbitrary
       transitions  <- [Gen (x,y) | x <- sublistOf states, y <- sublistOf states]
       actions      <- arbitrary
       initStates   <- sublistOf states
       return (LTS (nub states) (nub transitions) (nub actions) (nub initStates))
      
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
                    
prop_witnessPath    lts s =
    elem s (getStates lts) ==>
        let path = witnessPath lts s
            states = getStates lts      in 
            case path of
                [] -> False == isReachable lts s
                x:xs -> 
                    elem x initStates &&                                  -- first states is an nitial state 
                    all (`elem` transitions) pathTransitions &&             -- all path edges already exist in the LTS
                    all (\((u0,v0),(u1,v1)) -> v0 == u1) adjTransitions     -- adjacent transitions form a path
                    where   initStates          = getInitStates lts
                            transitions         = getTransitions lts
                            pathTransitions     = zip (x:xs) xs
                            adjTransitions      = zip pathTransitions (tail pathTransitions)

return [] -- need this for GHC 7.8


runTests = $quickCheckAll
-}