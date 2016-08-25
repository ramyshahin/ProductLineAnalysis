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
neighborsLifted :: Lifted [Transition] -> StateLifted -> Lifted [State] 
neighborsLifted [([],_)] s = liftT []
neighborsLifted ts' s =
          let  t = headLifted ts'
               ts = tailLifted ts'
          in
               condLifted (apply2 (liftT (==)) (sourceLifted t) s)
               (consLifted (targetLifted t) (neighborsLifted ts s))
               (neighborsLifted ts s)

-- lifted Depth-first Search
{-
dfsLifted ::  [TransitionLifted]    ->      -- graph edges
             Lifted [StateLifted]  ->      -- visited states
             StateLifted           ->      -- target node
             StateLifted           ->      -- source node
             Lifted [StateLifted]          -- returns the path from source to target
        
dfsLifted edges visited target src =
    let visited' = apply2 (lift True (++)) visited (lift True [src])
    in  condLifted (apply2 (lift True (==)) target src) visited'
        (let ns = (apply2 (lift True (\\)) (neighborsLifted edges src) visited')
             in headLifted (map (\n -> let r = (dfsLifted edges visited' target n)
                                       in  condLifted (apply (lift True null) r)
                                                      [([],True)]
                                                      (consLifted src r))
                            ns))
-}