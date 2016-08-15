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

-- lifted Act type
type ActLifted = Lifted Act

-- lifted Transition constructor
consTransitionLifted :: PresenceCondition -> StateLifted -> StateLifted -> ActLifted -> TransitionLifted
consTransitionLifted pc s0 s1 a = apply3 (lift pc Transition) s0 s1 a

-- lifted Transition accessors
source :: TransitionLifted -> StateLifted
source = apply (lift True LTS.source)

target :: TransitionLifted -> StateLifted
target = apply (lift True LTS.target)

action :: TransitionLifted -> ActLifted
action = apply (lift True LTS.action)

-- lifted neighbors
neighborsLifted :: [TransitionLifted] -> StateLifted -> [StateLifted]
neighborsLifted ts s = [(LiftedLTS.target t) | t <- ts, (LiftedLTS.source t) == s]
