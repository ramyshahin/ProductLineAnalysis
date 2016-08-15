-------------------------------------------------------------------------------
-- LiftedLTS_test.hs
-- Lifted Labeled Transition System (LTS) unit tests
-- Ramy Shahin - August 10th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module LiftedLTS_test where
--import Test.QuickCheck
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
