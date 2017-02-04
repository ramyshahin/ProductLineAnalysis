-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module LiftedLTS where
import LTS
import Data.List
import Data.Tree
import Control.Applicative
import SPL
import Debug.Trace
import Control.Exception

type State' = Var State
type Guard' = Var Guard
type Action' = Var Action

mkAction = cliftV2 Action

type Transition' = Var Transition

mkTransition = cliftV3 Transition
source' = cliftV source
target' = cliftV target
act' = cliftV act

type LTS' = Var LTS

mkLTS = cliftV4 LTS

neighbors' :: Var [Transition] -> State' -> Var [State]

neighbors' = cliftV2 neighbors

isReachable' = cliftV2 isReachable

witnessPath' = cliftV2 witnessPath

dfs' = cliftV4 dfs

