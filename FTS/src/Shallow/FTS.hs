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

type VState = Var State
type VGuard = Var Guard
type VAction = Var Action

mkAction = liftV2 Action

type VTransition = Var Transition

mkTransition = liftV3 Transition
vsource = liftV source
vtarget = liftV target
vact = liftV act

type VLTS = Var LTS

mkLTS = liftV4 LTS

vneighbors :: Var [Transition] -> VState -> Var [State]

vneighbors = liftV2 neighbors

visReachable = liftV2 isReachable

vwitnessPath = liftV2 witnessPath

vdfs = liftV4 dfs

