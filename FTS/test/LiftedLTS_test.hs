-------------------------------------------------------------------------------
-- LiftedLTS_test.hs
-- Lifted Labeled Transition System (LTS) unit tests
-- Ramy Shahin - August 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module LiftedLTS_test where
--import Test.QuickCheck
import LTS
import LiftedLTS
import Data.List
import Variability

la = [(3,True),(-3,True),(-9,True),(0,True)]
lb = [(6,True),(9,True),(-2,True),(0,True),(1,True)]

-- example: lifting abs
absLifted :: Lifted (Integer -> Integer)
absLifted = lift True abs

-- example: lifting addition
addLifted :: Lifted (Integer -> Integer -> Integer)
addLifted = lift True (+)

states = [1..5]
ts =          [Transition 1 2 0,
               Transition 2 3 0,
               Transition 2 6 0,
               Transition 3 6 0,
               Transition 6 5 0,
               Transition 5 4 0,
               Transition 6 7 0,
               Transition 7 5 0]

statesLifted = liftT states

tsLifted = liftT ts

n1 = neighborsLifted tsLifted [(1,True)]
n2 = neighborsLifted tsLifted [(2,True)]
n3 = neighborsLifted tsLifted [(3,True)]
n4 = neighborsLifted tsLifted [(4,True)]
n5 = neighborsLifted tsLifted [(5,True)]
