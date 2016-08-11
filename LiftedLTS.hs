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

-- abstract State type
type StateLifted = Lifted State

type TransitionLifted = Lifted Transition

consLiftedTransition :: StateLifted -> StateLifted -> Act -> PresenceCondition -> TransitionLifted
consLiftedTransition s0 s1 a pc = apply3 pc Transition s0 s1 (liftValue a)

{-
getSource :: TransitionLifted -> StateLifted
getSource t = [(s0,pc) | ((s0,_,_),pc) <- t]

getTarget :: TransitionLifted -> StateLifted
getSource t = [(s1,pc) | ((_,s1,_),pc) <- t]

getAction :: TransitionLifted -> StateLifted
-}

neighborsLifted :: [TransitionLifted] -> StateLifted -> [StateLifted]
neighborsLifted ts s = [(target t) | t <- ts, apply2 (==) (source t) s]