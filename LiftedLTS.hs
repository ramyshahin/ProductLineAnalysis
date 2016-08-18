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
sourceLifted :: TransitionLifted -> StateLifted
sourceLifted = apply (lift True LTS.source)

targetLifted :: TransitionLifted -> StateLifted
targetLifted = apply (lift True LTS.target)

actionLifted :: TransitionLifted -> ActLifted
actionLifted = apply (lift True LTS.action)

-- lifted neighbors
neighborsLifted :: [TransitionLifted] -> StateLifted -> Lifted [StateLifted]
neighborsLifted [] s = [([],True)]
neighborsLifted ts' s =
          let  t = head ts'
               ts = tail ts'
          in
               condLifted (apply2 (lift True (==)) (sourceLifted t) s)
               (consLifted (targetLifted t) (neighborsLifted ts s))
               (neighborsLifted ts s)

-- lifted dfs
{-dfsLifted :: [TransitionLifted]    ->      -- graph edges
             [StateLifted]         ->      -- visited states
             StateLifted           ->      -- target node
             StateLifted           ->      -- source node
             [StateLifted]                 -- returns the path from source to target
        
dfsLifted edges visited target src =
    let visited' = apply2 (++) visited [src]
    in  if (target == src) then visited'
    else let ns = (neighbors edges src) \\ visited'
         in head (map (\n -> let r = (dfs edges visited' target n)
                             in  if (null r) then [] else (src : r)) ns)
-}