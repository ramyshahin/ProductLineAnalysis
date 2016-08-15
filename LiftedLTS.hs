-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module LiftedLTS where
import LTS
import Data.List
import Data.Tree
import Variability

-- lifted State type
type StateLifted = Lifted State

-- lifted Transition type
type TransitionLifted = Lifted Transition

-- lifted Transition constructor
consLiftedTransition :: StateLifted -> StateLifted -> Act -> PresenceCondition -> TransitionLifted
consLiftedTransition s0 s1 a pc = apply3 pc Transition s0 s1 (liftValue a)

-- lifted neighbors function
-- neighbors ts s = [(target t) | t <- ts, (source t) == s]

neighborsLifted :: [TransitionLifted] -> StateLifted -> [StateLifted]
neighborsLifted ts s = [map (\(t',pc) -> (target t',pc)) (filter (\(t',_) -> (source t' == s)) t) | t <- ts]