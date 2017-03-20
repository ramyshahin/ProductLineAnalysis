-------------------------------------------------------------------------------
-- LiftedLTS.hs
-- Lifted Labeled Transition System (LTS) library
-- Ramy Shahin - July 20th 2016
-------------------------------------------------------------------------------
module Shallow.FTS where
import LTS
import Data.List
--import Data.Tree
import Control.Applicative
import SPL
import Debug.Trace
import Control.Exception

mkVarStates :: Int -> Int -> Var [State]
mkVarStates begin end =
    if begin > end then e else (mkVarT s) |:| mkVarStates (begin + 1) end
    where s = "s" ++ (show begin)

type State' = Var State
type Guard' = Var Guard
type Action' = Var Action

mkAction = liftV2 Action

type Transition' = Var Transition

mkTransition = liftV3 Transition
source' = liftV source
target' = liftV target
act' = liftV act

type LTS' = Var LTS

mkLTS = liftV4 LTS

neighbors' :: Var [Transition] -> State' -> Var [State]

neighbors' = liftV2 neighbors

isReachable' = liftV2 isReachable

witnessPath' = liftV2 witnessPath

dfs' = liftV4 dfs

